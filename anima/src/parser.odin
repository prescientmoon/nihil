#+private file
package anima

import "core:fmt"
import "core:log"
import "core:mem"
import "core:mem/virtual"
import "core:reflect"
import "core:strings"
import "core:unicode/utf8"

// Basics
// {{{ Paring-related types
// This is technically wasting some memory by storing the file pointer twice and
// whatnot, but it does not matter in the grand scheme of things.
@(private = "package")
Source_Range :: [2]Source_Loc

@(private = "package")
Error_Location :: union {
  Path__Absolute,
  Path__Input,
  Path__Output,
  ^File,
  Source_Loc,
  Token,
  Source_Range,
}

@(private = "package")
Error :: struct {
  loc: Error_Location,
	msg: string,
}

@(private = "package")
File :: struct {
  path:   Path__Input,
  source: string
}

// This is a bit fatter than we could get away with, but it doesn't really 
// matter, and having everything in one place makes a lot of stuff easier.
@(private = "package")
Source_Loc :: struct {
  file:  ^File,
	index: uint,
	line:  uint,
	col:   uint,
}

// NOTE: the "content" property might be null for certain tokens (i.e. EOF).
@(private = "package")
Token :: struct {
	from:    Source_Loc,
	content: string,
	kind:    Token_Kind,
}

Token_Kind :: enum u8 {
	None = 0,
	Space,      // One or more spaces
	Newline,    // Newlines (\r is not kept around at the moment)
	Word,       // A contiguous sequence of everything else
	Apparition, // \<word>
	Bang,       // !
	LCurly,     // {
	RCurly,     // }
	Eof,        // Special token emitted once there's nothing more to consume
}
// }}}

// Lexing
// {{{ The lexer type
// The lexer for the anima language. The "curr" propery contains the rune
// currently being focused. The lexer works in a streaming fashion, by
// incrementally moving the focus forwards.
//
// The "next_index" is required for handling multi-byte characters.
//
// If any of the underlying functions error out, a proper error string (together
// with the source location) will get saved in the "error" property, and the
// "ok" boolean of the given function will be returned as "false".
Lexer :: struct {
	forever:         mem.Allocator,
	source:          string,
	pos:             Source_Loc,
	curr:            rune,
	next_index:      uint,
	error:           struct {
		pos: Source_Loc,
		msg: string,
	},
}

@(private = "package")
lexer__make :: proc(
  file: ^File, forever: mem.Allocator,
) -> (lexer: Lexer, ok: bool) {
	lexer = Lexer {
    forever = forever,
		source = file.source,
		pos = Source_Loc{file = file, line = 1, col = 0, index = 0},
		curr = 0,
		next_index = 0,
	}

	advance_rune(&lexer) or_return

	return lexer, true
}
// }}}
// {{{ Lexing helpers
@(require_results)
advance_rune :: proc(lexer: ^Lexer) -> (ok: bool) {
	if lexer.next_index >= len(lexer.source) {
		lexer.pos.index = len(lexer.source)

		if lexer.curr == '\n' {
			lexer.pos.line += 1
			lexer.pos.col = 1
		}

		lexer.next_index = ~uint(0)
		lexer.curr = {}
		return true
	}

	lexer.pos.index = lexer.next_index
	if lexer.curr == '\n' {
		lexer.pos.col = 1
		lexer.pos.line += 1
	} else {
		lexer.pos.col += 1
	}

	r, w := rune(lexer.source[lexer.next_index]), 1
	switch {
	case r == 0:
		lexer.error = {lexer.pos, "Illegal character NUL"}
		return false
	case r >= utf8.RUNE_SELF:
		r, w = utf8.decode_rune_in_string(lexer.source[lexer.next_index:])
		if r == utf8.RUNE_ERROR && w == 1 {
			lexer.error = {lexer.pos, "Illegal UTF-8 encoding"}
			return false
		} else if r == utf8.RUNE_BOM && lexer.next_index > 0 {
			lexer.error = {lexer.pos, "Illegal byte order mark"}
			return false
		}
	}

	lexer.next_index += uint(w)
	lexer.curr = r

	return true
}

// Look at the lexer's next rune without actively focusing it.
peek_rune :: proc(lexer: Lexer) -> rune {
	copy := lexer
	ok := advance_rune(&copy)
	if ok {
		return copy.curr
	} else {
		return {}
	}
}

// Check whether the lexer's focused rune is a valid word character. This will
// also handle escaping any of the special syntax characters like { or \.
next_rune_is_text_char :: proc(lexer: Lexer) -> (width: uint) {
	nch := peek_rune(lexer)
	switch lexer.curr {
	case {}:
		return 0
	case '\\':
		switch nch {
		case '{', '}', '!', '\\', '\n', '\r', '\t', ' ':
			return 2
		case:
			return 0
		}
	case '{', '}', '!', '\n', '\r', '\t', ' ':
		return 0
	case:
		return 1
	}
}
// }}}
// {{{ Word char consumer
// Consumes any character allowed inside a word (see `next_rune_is_text_char`),
// then resolves any escaped characters. The output string will only cause an
// allocation if character escapes are encountered.
consume_word_chars :: proc(lexer: ^Lexer) -> (s: string, ok: bool) {
	copy := lexer^

	no_escapes := true

	// Traverse word once in order to compute the maximum size taken by the output
	// string
	for {
		width := next_rune_is_text_char(copy)
		(width > 0) or_break
		for _ in 0 ..< width do advance_rune(&copy) or_return
		no_escapes &&= width == 1
	}

	if no_escapes {
		string := lexer.source[lexer.pos.index:copy.pos.index]
		lexer^ = copy
		return string, true
	}

	builder := strings__fixed_builder(
		copy.pos.index - lexer.pos.index,
		lexer.forever,
	)

	for {
		width := next_rune_is_text_char(lexer^)

		assert(width <= 2)
		(width > 0) or_break

		// Escaped: skip the backslash
		if width == 2 do advance_rune(lexer) or_return

		strings.write_rune(&builder, lexer.curr)
		advance_rune(lexer) or_return
	}

	return strings.to_string(builder), true
}
// }}}
// {{{ Lexing loop entrypoint
@(private = "package")
tokenize :: proc(lexer: ^Lexer) -> (tok: Token, ok: bool) {
	// Skip all \r characters. Such characters cannot currently be escaped. I
	// don't use windows, so I do not care in the end.
	for lexer.curr == '\r' do advance_rune(lexer) or_return

	tok.from = lexer.pos
	tok.kind = .Eof

	char := lexer.curr
	switch char {
	case {}:
		break
	case '{', '}', '!':
		@(rodata, static)
		KIND_TABLE: [128]Token_Kind = {
			'{' = .LCurly,
			'}' = .RCurly,
			'!' = .Bang,
		}

		tok.kind = KIND_TABLE[char]
		advance_rune(lexer) or_return
		tok.content = lexer.source[tok.from.index:][:1]
	case '\n':
		tok.kind = .Newline
		advance_rune(lexer) or_return
	case '\t', ' ':
		tok.kind = .Space
		curr := lexer.pos.index
		for lexer.curr == '\t' || lexer.curr == ' ' do advance_rune(lexer) or_return
		tok.content = lexer.source[curr:lexer.pos.index]
	case:
		if next_rune_is_text_char(lexer^) > 0 {
			tok.kind = .Word
			tok.content = consume_word_chars(lexer) or_return
		} else {
			assert(lexer.curr == '\\')
			tok.kind = .Apparition
			advance_rune(lexer) or_return
			tok.content = consume_word_chars(lexer) or_return
		}
	}

	return tok, true
}
// }}}
// {{{ Running the full blown lexer
Indented_Token :: struct {
	using token: Token,
	indentation: uint,
}

Tokens :: Exparr(Indented_Token, 10)

// Allocates the token array on the stack arena.
@(private = "package")
parser__lex :: proc(site: ^Site, file: ^File) -> (tokens: Tokens, ok: bool) {
  forever := site__alloc(site, .Forever)
	lexer := lexer__make(file, forever) or_return
  tokens.allocator = site__alloc(site, .Stack)

	for {
    tok: Token
		tok, ok = tokenize(&lexer)

		if !ok {
			tok.from = lexer.error.pos
			site__error(site, tok, lexer.error.msg)
			return
		}

		itok := Indented_Token {
			token       = tok,
			indentation = tok.from.col,
		}

		push(&tokens, itok)

		if tok.kind == .Eof do break
	}

	current_indentation: uint = 0
	for i := tokens.len - 1; int(i) >= 0; i -= 1 {
		tok := get(tokens, i)
		if tok.kind == .Space || tok.kind == .Newline {
			tok.indentation = current_indentation
		} else {
			current_indentation = tok.indentation
		}
	}

	site.statistics.tokens += tokens.len
  site__update_stack_stats(site)

	return tokens, true
}
// }}}

// Parsing
// {{{ The parser type
// If codecs are templates for parsers, a codec instance is the actual parser,
// together with its state.
Parser :: struct {
  // Data given to us from the outside
  site:          ^Site,
	codec:         ^Codec, // Dictates what parsing will look like
	tokens:        Tokens, // Read-only
	token:         ^uint, // The index of the current token
  ok:            ^bool, // Set to false if we've ever emitted an error

  // Data that is local to the current instance
	output:       rawptr,
	in_paragraph: bool,
  scratch:      bool, // Are we (possibly deep) inside a scratch focus codec?
	document:     rawptr, // Top-level context any function can access

  // Data about the surrounding apparition
  indentation:      uint,
  surrounded_at:    Source_Loc,
  surrounding_kind: enum { Indented, Bracketed },

	// The capacity for this list is computed at the start of the block, and its
	// allocator is set to the panic allocator. We could instead store this as a
	// capacity+slice combination, but the extra pointers required to store the
	// panic allocator doesn't matter in the grand scheme of things.
	//
	// A more efficient representation would use a bitset, but that's not worth
	// spending time on right now.
	completed_codecs: ^[dynamic]^Codec,
}
// }}}
// {{{ Parser helpers
parser__get_pos :: proc(instance: Parser) -> Source_Loc {
  return get(instance.tokens, instance.token^).from
}

parser__advance :: proc(instance: Parser) {
	instance.token^ += 1
}

parser__get_token :: proc(instance: Parser) -> (tok: Token) {
	itok := get(instance.tokens, instance.token^)^

	if itok.indentation <= instance.indentation {
		return {from = tok.from}
	}

	// Paragraphs get terminated by consecutive newlines.
	if instance.in_paragraph && itok.kind == .Newline {
		offset: uint = 1
		for {
      index     := instance.token^ + offset
			next_itok := get(instance.tokens, index)^

			if next_itok.kind == .Space {
				offset += 1
			} else if next_itok.kind == .Newline {
				return {from = tok.from} // We dodged the bullet!
			} else {
				break
			}
		}
	}

	return itok.token
}
// }}}
// {{{ Codec evaluation
// Count the number of codecs in the current block that care about keeping track
// of whether they've been completed or not.
codec__count_completable :: proc(instance: Parser) -> uint {
	switch inner in instance.codec.data {
	case Codec__Space, Codec__Constant, Codec__Text, Codec__Raw, Codec__At, nil:
		return 0
	case Codec__Focus:
		inner_instance := instance
		inner_instance.codec = inner.inner
		return codec__count_completable(inner_instance)
	case Codec__Tracked:
		inner_instance := instance
		inner_instance.codec = inner.inner
		return codec__count_completable(inner_instance) + 1
	case Codec__Loop:
		inner_instance := instance
		inner_instance.codec = cast(^Codec)inner
		return codec__count_completable(inner_instance)
	case Codec__Paragraph:
		inner_instance := instance
		inner_instance.codec = cast(^Codec)inner
		return codec__count_completable(inner_instance)
	case Codec__Seq:
		total: uint = 0
		for &codec in inner {
			inner_instance := instance
			inner_instance.codec = &codec
			total += codec__count_completable(inner_instance)
		}

		return total + len(inner)
	case Codec__Sum:
		total: uint = 0
		for &codec in inner {
			inner_instance := instance
			inner_instance.codec = &codec
			total += codec__count_completable(inner_instance)
		}

		return total
	}

	panic("impossible")
}

codec__is_completed :: proc(instance: Parser) -> bool {
	for completed in instance.completed_codecs {
		if completed == instance.codec do return true
	}

	return false
}

codec__mark_completed :: proc(instance: Parser, could_be_completed := true) {
	if could_be_completed && codec__is_completed(instance) do return
	append_elem(instance.completed_codecs, instance.codec)
}

codec__make_completed_state :: proc(instance: ^Parser) {
	allocator := site__alloc(instance.site, .Stack)

	completed_codecs := make_dynamic_array_len_cap(
		[dynamic](^Codec),
		0,
		codec__count_completable(instance^),
		allocator,
	)

	completed_codecs.allocator = mem.panic_allocator()
	instance.completed_codecs = new_clone(completed_codecs, allocator)
}

parser__skip_spaces :: proc(instance: Parser) -> (consumed: bool) {
	for {
		tok := parser__get_token(instance)
		(tok.kind == .Space || tok.kind == .Newline) or_break
		parser__advance(instance)
		consumed = true
	}

	return consumed
}

codec__eval_instance :: proc(instance: Parser) -> (consumed: bool) {
	instance.site.statistics.codec_evaluations += 1
	switch inner in instance.codec.data {
	case Codec__Space:
		consumed = parser__skip_spaces(instance)

    if consumed {
      size := reflect.size_of_typeid(instance.codec.type)
      mem.copy(instance.output, inner, size)
    }

    return consumed
	case Codec__Text:
		log.assertf(
			instance.codec.type == string,
			"Expected codec of type string. Got %v instead.",
			instance.codec.type,
		)

		tok := parser__get_token(instance)
		if tok.kind != .Word && tok.kind != .Bang do return false
		parser__advance(instance)
		mem.copy(instance.output, &tok.content, size_of(string))
		return true
	case Codec__Raw:
    site__frame(instance.site)

		log.assertf(
			instance.codec.type == string,
			"Expected codec of type string. Got %v instead.",
			instance.codec.type,
		)

    forever_alloc := site__alloc(instance.site, .Forever)
    temp_alloc    := site__alloc(instance.site, .Stack)
    lines: Exparr(string)
    lines.allocator = temp_alloc

    builder: strings.Builder
    strings.builder_init_none(&builder, temp_alloc)
    
    should_continue := true
    for should_continue {
      for {
        tok := parser__get_token(instance) 
        if tok.kind == .None do should_continue = false
        switch instance.surrounding_kind {
        case .Indented:
        case .Bracketed:
          if tok.kind == .RCurly do should_continue = false
        }

        if !should_continue do break

        parser__advance(instance)
        if tok.kind == .Newline do break
        fmt.sbprint(&builder, tok.content)
      }

      clone, err := strings.clone(strings.to_string(builder), temp_alloc)
      log.assert(err == nil)
      strings.builder_reset(&builder)
      push(&lines, clone)
    }

    min_indent := max(uint)
    for iter := iter__mk(lines); line in iter__next(&iter) {
      indent: uint = 0
      non_empty := false
      for char in line^ {
        if char == ' ' {
          indent += 1
        } else {
          non_empty = true
          break
        }
      }

      non_empty or_continue
      if min_indent > indent do min_indent = indent
    }

    for iter := iter__mk(lines); line, i in iter__next(&iter) {
      sliced := len(line^) >= int(min_indent) \
        ? strings.trim_right_space(line^[min_indent:]) : ""
      if i > 0 do strings.write_rune(&builder, '\n')
      strings.write_string(&builder, sliced)
    }

    clone, err := strings.clone(strings.to_string(builder), forever_alloc)
    log.assert(err == nil)

    trimmed := strings.trim_space(clone)
    mem.copy(instance.output, &trimmed, size_of(string))

		return len(clone) > 0
	case Codec__Constant:
		tok := parser__get_token(instance)
		if tok.kind != .Apparition do return false
		if tok.content != inner.name do return false
		parser__advance(instance)
		mem.copy(instance.output, inner.value, reflect.size_of_typeid(instance.codec.type))
		return true
	case Codec__Focus:
		site__frame(instance.site, instance.scratch)
    defer site__update_stack_stats(instance.site)

    inner_alloc := site__alloc(
      instance.site,
      .Stack if instance.scratch else .Forever
    )

    temp_alloc := site__alloc(instance.site, .Stack)
		inner_output := mem__reflected_new(inner.inner.type, temp_alloc)
    kit := Lens_Kit {
      outer           = instance.output,
      inner           = inner_output,
      user_data       = inner.user_data,
      document        = instance.document,
      mode            = .Project,
      allocator       = inner_alloc,
      temp_allocator  = temp_alloc,
      surrounded_at   = instance.surrounded_at,
      error_allocator = site__alloc(instance.site, .Forever),
    }

    kit.errors.allocator = kit.temp_allocator

		inner.lens(&kit)
    pos__pre := parser__get_pos(instance)
    if kit.errors.len == 0 {
      if kit.ignored do return false
      inner_instance := instance
      inner_instance.codec = inner.inner
      inner_instance.output = inner_output
      inner_instance.document = kit.document
      inner_instance.scratch ||= inner.scratch

      consumed = codec__eval_instance(inner_instance)
      if consumed {
        kit.mode = .Inject
        kit.ignored = !consumed
        inner.lens(&kit)
      }

      if kit.errors.len == 0 do return consumed && !kit.ignored
    }

    pos__post := parser__get_pos(instance)
    loc := Source_Range{pos__pre, pos__post}
    for iter := iter__mk(kit.errors); msg in iter__next(&iter) {
      instance.ok^ = false
      site__error(instance.site, loc, msg^)
    }

    return consumed
	case Codec__Seq:
    // Say we have a codec sequence "AB" (the general case follows by induction).
    // We want to run A if possible, but not if B has ever run. If A does not
    // consume anything, we fall back to B.
    //
    // Something to keep in mind is that the "completed" in things like
    // "codec__is_completed" actually means "consumed" (I need to change the
    // naming at some point).
    tok := instance.token^

    earliest_thats_never_consumed := len(inner)
    for &codec, i in inner {
			inner_instance := instance
			inner_instance.codec = &codec
      if !codec__is_completed(inner_instance) {
        earliest_thats_never_consumed = i
        break
      }
    }

    // If the "C" in "ABC" has never consumed anything, then "B" might still
    // have stuff left to parse out. We've initialized
    // "earliest_thats_never_consumed" to "len(inner)" such that this will
    // reslove to the last codec if every single codec has consumed.
    earliest_runnable := earliest_thats_never_consumed == 0 \
      ? 0 : earliest_thats_never_consumed - 1

    for &codec, i in inner[earliest_runnable:] {
			inner_instance := instance
			inner_instance.codec = &codec

			consumed := codec__eval_instance(inner_instance)
      if consumed || instance.token^ > tok {
        if consumed && i >= earliest_thats_never_consumed {
          codec__mark_completed(inner_instance, could_be_completed = false)
        }

        return consumed
      }
		}

		return false
	case Codec__Sum:
    tok := instance.token^
		for &codec in inner {
			inner_instance := instance
			inner_instance.codec = &codec

			consumed := codec__eval_instance(inner_instance)
      if consumed || instance.token^ > tok do return consumed
		}

		return false
	case Codec__At:
		tok := parser__get_token(instance)
		if tok.kind != .Apparition do return false
		if tok.content != inner.name do return false
		parser__advance(instance)

		site__frame(instance.site, instance.scratch)

		inner_instance := instance
		inner_instance.codec = inner.inner
		inner_instance.in_paragraph = false
    inner_instance.surrounded_at = tok.from
		codec__make_completed_state(&inner_instance)

    // Unlike the parser's .surrounding_kind, this has an explicit .Ambient
    // branch.
    Kind :: enum { Bracketed, Indented, Ambient }
    kind: Kind

		next_tok := parser__get_token(instance)
		#partial switch next_tok.kind {
		case .LCurly:
			parser__advance(instance)
			inner_instance.surrounding_kind = .Bracketed
			inner_instance.indentation = instance.indentation
			kind = .Bracketed
		case .Bang:
			parser__advance(instance)
			inner_instance.indentation = instance.indentation
			kind = .Ambient
		case:
			inner_instance.surrounding_kind = .Indented
			inner_instance.indentation = tok.from.col
			kind = .Indented
		}

		codec__eval_instance(inner_instance)
		last_tok := parser__get_token(instance)

		if kind == .Bracketed {
			if last_tok.kind == .RCurly {
				parser__advance(instance)
			} else {
        instance.ok^ = false
				site__errorf(
					instance.site,
					last_tok,
					"I don't know how to parse this token. Common causes:\n" +
          "- missing closing brackets\n" +
          "- trying to use a unique apparition more than once per scope",
				)
			}
		}

    codec__check_flags(tok, inner_instance)
		return true
	case Codec__Tracked:
		inner_instance := instance
		inner_instance.codec = inner.inner

		is_completed := codec__is_completed(inner_instance)
		if is_completed && .Unique in inner.flags do return false

		consumed = codec__eval_instance(inner_instance)
		if !is_completed && consumed {
			codec__mark_completed(inner_instance, could_be_completed = false)
		}

		return consumed
	case Codec__Loop:
		inner_instance := instance
		inner_instance.codec = cast(^Codec)inner

		tok := instance.token^
		for {
      consumed_now := codec__eval_instance(inner_instance)
		  (consumed_now || instance.token^ > tok) or_break
      consumed ||= consumed_now
		  tok = instance.token^
		}

		return consumed
	case Codec__Paragraph:
		inner_instance := instance
		inner_instance.codec = cast(^Codec)inner
		inner_instance.in_paragraph = true

		return codec__eval_instance(inner_instance)
	case nil:
		log.panic("Cannot evaluate the nil codec.")
	}

	log.panic("impossible", instance.codec)
}

@(private = "package")
codec__check_flags :: proc(loc: Error_Location, instance: Parser) {
  inner_instance := instance
  switch inner in instance.codec.data {
	case Codec__Space, Codec__Constant, Codec__Text, Codec__Raw, Codec__At, nil:
  case Codec__Focus: 
    inner_instance.codec = inner.inner
    codec__check_flags(loc, inner_instance)
  case Codec__Loop: 
    inner_instance.codec = cast(^Codec)inner
    codec__check_flags(loc, inner_instance)
  case Codec__Paragraph: 
    inner_instance.codec = cast(^Codec)inner
    codec__check_flags(loc, inner_instance)
  case Codec__Seq: 
    for &codec in inner {
      inner_instance.codec = &codec
      codec__check_flags(loc, inner_instance)
    }
  case Codec__Sum: 
    for &codec in inner {
      inner_instance.codec = &codec
      codec__check_flags(loc, inner_instance)
    }
  case Codec__Tracked:
    inner_instance.codec = inner.inner
    if .Required in inner.flags && !codec__is_completed(inner_instance) {
      instance.ok^ = false
      site__errorf(
        instance.site,
        loc,
        "Argument \"%v\" is required in this scope.",
        inner.name,
      )
    } else {
      codec__check_flags(loc, inner_instance)
    }
  }
}
// }}}
// {{{ Paring entrypoint
@(private = "package")
parser__eval :: proc(
  site: ^Site, codec: ^Codec, file: ^File, output: rawptr
) -> (ok: bool)  {
  site__frame(site)

  tokens := parser__lex(site, file) or_return
	instance := Parser {
    site     = site,
		codec    = codec,
		output   = output,
    tokens   = tokens,
    // We could keep these on the proper stack but idrc
    token    = new(uint, site__alloc(site, .Stack)),
    ok       = new_clone(true, site__alloc(site, .Stack)),
	}

	codec__make_completed_state(&instance)

	_ = codec__eval_instance(instance)
  if !instance.ok^ do return

  tok := parser__get_token(instance)
  if tok.kind != .Eof {
    site__error(site, tok, "Unexpected token. Expected end of file.")
    return
  }

  codec__check_flags(file, instance)
  if !instance.ok^ do return

  return true
}
// }}}
