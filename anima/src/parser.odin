#+private file
package anima

import "core:fmt"
import "core:log"
import "core:mem"
import "core:mem/virtual"
import "core:os"
import "core:path/slashpath"
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
  Source_Loc,
  Token,
  Source_Range,
}

@(private = "package")
Error :: struct {
  loc: Error_Location,
	msg: string,
}

// This is a bit fatter than we could get away with, but it doesn't really
// matter, and having everything in one place makes a lot of stuff easier.
@(private = "package")
Source_Loc :: struct {
  path:  Path__Input,
	index: uint,
	line:  uint,
	col:   uint,
}

// NOTE: the "content" property might be null for certain tokens (i.e. EOF).
@(private = "package")
Token :: struct {
	from:        Source_Loc,
	content:     string,
	kind:        Token_Kind,
	indentation: uint,
}

@(private = "package")
Token_Kind :: enum u8 {
	None = 0,
	Space,        // One or more spaces
	Newline,      // Newlines (\r is not kept around at the moment)
	Word,         // A contiguous sequence of everything else
	Apparition,   // \<word>
	Bang,         // !
	LCurly,       // {
	RCurly,       // }
	LSquare,      // [
	RSquare,      // ]
  Asterisk,     // *
  Underscore,   // _
  Backtick,     // `
  Quote,        // "
  Ellipsis,     // ...
  Tilde,        // ~
  GT,           // >
  Bar,          // |
  Dollar,       // $
  Hash,         // #
  Double_Hash,  // ##
  Triple_Hash,  // ###
  Double_Slash, // //
	Eof,          // Special token emitted once there's nothing more to consume
}

// Vislaul representation
@(rodata, private = "package")
Token_Kind__Symbol := [Token_Kind]string {
	.None         = "none",
	.Space        = "<space>",
	.Newline      = "<newline>",
	.Word         = "<word>",
	.Apparition   = "<apparition>",
	.Bang         = "!",
	.LCurly       = "{",
	.RCurly       = "}",
	.LSquare      = "[",
	.RSquare      = "]",
  .Asterisk     = "*",
  .Underscore   = "_",
  .Backtick     = "`",
  .Quote        = "\"",
  .Ellipsis     = "...",
  .Tilde        = "~",
  .GT           = ">",
  .Bar          = "|",
  .Dollar       = "$",
  .Hash         = "#",
  .Double_Hash  = "##",
  .Triple_Hash  = "###",
  .Double_Slash = "//",
	.Eof          = "<eof>",
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
  path: Path__Input, source: string, forever: mem.Allocator,
) -> (lexer: Lexer) {
	lexer = Lexer {
    forever = forever,
		source = source,
		pos = Source_Loc{path = path, line = 1, col = 0, index = 0},
		curr = 0,
		next_index = 0,
	}

	advance_rune(&lexer)

	return lexer
}
// }}}
// {{{ Lexing helpers
advance_rune :: proc(lexer: ^Lexer) {
	if lexer.next_index >= len(lexer.source) {
		lexer.pos.index = len(lexer.source)

		if lexer.curr == '\n' {
			lexer.pos.line += 1
			lexer.pos.col = 1
		}

		lexer.next_index = ~uint(0)
		lexer.curr = {}
    return
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
	case r >= utf8.RUNE_SELF:
		r, w = utf8.decode_rune_in_string(lexer.source[lexer.next_index:])
		if r == utf8.RUNE_ERROR && w == 1 {
			lexer.error = {lexer.pos, "Illegal UTF-8 encoding"}
		} else if r == utf8.RUNE_BOM && lexer.next_index > 0 {
			lexer.error = {lexer.pos, "Illegal byte order mark"}
		}
	}

  if lexer.error == {} {
    lexer.next_index += uint(w)
    lexer.curr = r
  } else {
    lexer.curr = {}
  }
}

lexer__rune :: proc(lexer: ^Lexer, r: rune) -> bool {
  if lexer.curr == r {
    advance_rune(lexer)
    return true
  } else {
    return false
  }
}

lexer__expect :: proc(lexer: ^Lexer, str: string) -> bool {
  slice := lexer.source[lexer.pos.index:]
  (len(slice) >= len(str)) or_return
  (slice[:len(str)] == str) or_return

  target := lexer.pos.index + len(str)

  for lexer.pos.index < target {
    // We guarantee no unicode decoding errors!
    advance_rune(lexer)
  }

  log.assert(lexer.pos.index == target)

  return true
}
// }}}
// {{{ Lexing loop entrypoint
@(private = "package")
tokenize :: proc(lexer: ^Lexer, tokens: ^Tokens) {
  for {
    // Skip all \r characters. Such characters cannot currently be escaped. I
    // don't use windows, so I do not care in the end.
    for lexer__rune(lexer, '\r') {}

    tok := Token {
      from = lexer.pos,
      indentation = lexer.pos.col,
    }

    escaped := lexer__rune(lexer, '\\')
    curr := lexer.pos.index

    switch {
    case lexer__rune(lexer, {}): tok.kind = .Eof
    case lexer__rune(lexer, '\\'): tok.kind = .Word // Escaped backslash (\\)
    case lexer__rune(lexer, '{'): tok.kind = .LCurly
    case lexer__rune(lexer, '}'): tok.kind = .RCurly
    case lexer__rune(lexer, '['): tok.kind = .LSquare
    case lexer__rune(lexer, ']'): tok.kind = .RSquare
    case lexer__rune(lexer, '*'): tok.kind = .Asterisk
    case lexer__rune(lexer, '_'): tok.kind = .Underscore
    case lexer__rune(lexer, '`'): tok.kind = .Backtick
    case lexer__rune(lexer, '"'): tok.kind = .Quote
    case lexer__rune(lexer, '!'): tok.kind = .Bang
    case lexer__rune(lexer, '~'): tok.kind = .Tilde
    case lexer__rune(lexer, '>'): tok.kind = .GT
    case lexer__rune(lexer, '|'): tok.kind = .Bar
    case lexer__rune(lexer, '$'): tok.kind = .Dollar
    case lexer__rune(lexer, '\n'): tok.kind = .Newline
    case lexer__rune(lexer, ' ') || lexer__rune(lexer, '\t'):
      tok.kind = .Space
      for lexer__rune(lexer, ' ') || lexer__rune(lexer, '\t') {}
    case lexer__expect(lexer, "..."): tok.kind = .Ellipsis
    case lexer__expect(lexer, "###"): tok.kind = .Triple_Hash
    case lexer__expect(lexer, "##"): tok.kind = .Double_Hash
    case lexer__rune(lexer, '#'): tok.kind = .Hash
    case lexer__expect(lexer, "//"): tok.kind = .Double_Slash
    case:
      tok.kind = .Word
      advance_rune(lexer)
      loop: for {
        switch lexer.curr {
        case {}, '\\', '{', '}', '[', ']', '*', '_', '`', '"', '!', '~',
             '>', '|', '$', '#', '.', '/', '\n', '\r', '\t', ' ':
          break loop
        case:
          advance_rune(lexer)
        }
      }
    }

    tok.content = lexer.source[curr:lexer.pos.index]

    if escaped {
      #partial switch tok.kind {
      case .Word:
        tok.kind = .Apparition
      case .Eof:
        tok.kind = .Word
        tok.content = "\\"
      case:
        tok.kind = .Word
      }
    }

    exparr__push(tokens, tok)

    if tok.kind == .Eof do break
  }
}
// }}}
// {{{ Running the full blown lexer
Tokens :: Exparr(Token, 10)

// Allocates the token array on the stack arena.
@(private = "package")
parser__lex :: proc(
  site: ^Site, path: Path__Input, source: string
) -> (tokens: Tokens, ok: bool) {
  forever := site__alloc(site, .Forever)
	lexer := lexer__make(path, source, forever)
  tokens.allocator = site__alloc(site, .Stack)

  tokenize(&lexer, &tokens)
  if lexer.error != {} {
    tok: Token
    tok.from = lexer.error.pos
    site__error(site, tok, lexer.error.msg)
    return
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
  site:   ^Site,
	codec:  ^Codec, // Dictates what parsing will look like
	tokens: Tokens, // Read-only
	token:  ^uint, // The index of the current token
  ok:     ^bool, // Set to false if we've ever emitted an error
  path:   Path__Input, // Where are we currently parsing?

  // Data that is local to the current instance
	output:       rawptr,
	in_paragraph: bool,
  can_import:   bool, // When false, a higher import point already exists
  scratch:      bool, // Are we (possibly deep) inside a scratch focus codec?
	document:     rawptr, // Top-level context any function can access

  // Data about the surrounding apparition
  indentation:              uint,
  surrounded_at:            Source_Loc,
  surrounding_kind:         enum { Indented, Bracketed, Delimited },
  surrounding_delim_closer: Token_Kind,

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
  if get(instance.tokens, instance.token^).kind != .Eof {
    instance.token^ += 1
  }
}

// Unlike the parser's .surrounding_kind, this has an explicit .Ambient
// branch.
Apparition_Kind :: enum { Bracketed, Indented, Delimited, Ambient }

parser__begin_apparition :: proc(
  instance: Parser, inner_instance: ^Parser, starter: Token
) -> (
  kind: Apparition_Kind
) {
  inner_instance.in_paragraph = false
  inner_instance.can_import   = true
  inner_instance.surrounded_at = starter.from
  next_tok := parser__get_token(instance)
  #partial switch next_tok.kind {
  case .LCurly:
    parser__advance(instance)
    inner_instance.surrounding_kind = .Bracketed
    kind = .Bracketed
  case .Bang:
    parser__advance(instance)
    kind = .Ambient
  case:
    inner_instance.surrounding_kind = .Indented
    inner_instance.indentation = starter.from.col
    kind = .Indented
  }

  return kind
}

parser__end_apparition :: proc(
  instance: Parser, kind: Apparition_Kind, starter: Token
) {
  last_tok := parser__get_token(instance)
  msg :: "I don't know how to parse this token. Common causes:\n" +
        "- missing closing brackets\n" +
        "- trying to use a unique apparition more than once per scope\n" +
        "Did you perhaps forget to close the apparition starting at %v?"

  if kind == .Bracketed {
    if last_tok.kind == .RCurly {
      parser__advance(instance)
    } else {
      instance.ok^ = false
      site__errorf(
        instance.site,
        last_tok,
        msg,
        Error_Location(starter),
      )
    }
  } else if kind == .Delimited {
    if last_tok.kind == instance.surrounding_delim_closer {
      parser__advance(instance)
    } else {
      log.error(last_tok.kind, instance.surrounding_delim_closer)
      instance.ok^ = false
      site__errorf(
        instance.site,
        last_tok,
        msg,
        Error_Location(starter),
      )
    }
  }
}

parser__get_token :: proc(
  instance: Parser, skip_comments := true
) -> (tok: Token) {
	itok := get(instance.tokens, instance.token^)^

	if itok.indentation <= instance.indentation {
		return {from = tok.from}
	}

	// Paragraphs get terminated by consecutive newlines. This will do a bunch of
  // repeated work for each space in a multi-space sequence, althhough the lexer
  // already merges those, so this shoudln't happen in practice.
	if instance.in_paragraph && itok.kind == .Newline {
		offset: uint = 1
		for {
			next_itok := get(instance.tokens, instance.token^ + offset)^

			if next_itok.kind == .Space {
				offset += 1
			} else if next_itok.kind == .Newline {
				return {from = tok.from} // We dodged the bullet!
			} else {
				break
			}
		}
	}

  if skip_comments && (
    itok.kind == .Double_Slash ||
    itok.kind == .Apparition && itok.content == "--"
  ) {
    parser__advance(instance)

		inner_instance := instance
		inner_instance.codec = nil

    kind := parser__begin_apparition(instance, &inner_instance, itok)

    loop: for {
      tok := parser__get_token(inner_instance, skip_comments = false)

      switch {
      case tok.kind == .None:
        break loop
      case inner_instance.surrounding_kind == .Bracketed && tok.kind == .RCurly:
        break loop
      case:
        parser__advance(inner_instance)
      }
    }

    parser__end_apparition(inner_instance, kind, itok)
		return parser__get_token(instance)
  }

	return itok
}
// }}}
// {{{ Completion tracking
// Count the number of codecs in the current block that care about keeping track
// of whether they've been completed or not.
codec__count_completable :: proc(instance: Parser) -> uint {
	switch inner in instance.codec.data {
  case Codec__Space, Codec__Constant, Codec__Token, Codec__Text, Codec__Raw,
       Codec__At, Codec__Leaded, Codec__Delimited, nil:
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

make_completion_state :: proc(instance: ^Parser) {
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
// }}}
// {{{ Text parsing
parser__parse_word :: proc(instance: Parser) -> (out: string, ok: bool) {
  tok := parser__get_token(instance)
  (tok.kind == .Word || tok.kind == .Bang) or_return
  parser__advance(instance)
  return tok.content, true
}

parser__parse_contiguous_text :: proc(
  instance: Parser
) -> (out: string, ok: bool) {
  chunks := Exparr(string) { allocator = site__alloc(instance.site, .Stack) }
  for chunk in parser__parse_word(instance) do push(&chunks, chunk)

  size: uint = 0
  for iter := iter__mk(chunks); chunk in iter__next(&iter) {
    size += len(chunk)
  }

  if chunks.len == 0 {
    return "", false
  } else if chunks.len == 1 {
    return get(chunks, 0)^, true
  }

  builder := strings__fixed_builder(size, site__alloc(instance.site, .Stack))
  for iter := iter__mk(chunks); chunk in iter__next(&iter) {
    strings.write_string(&builder, chunk^)
  }

  return strings.to_string(builder), true
}
// }}}
// {{{ Codec evaluation
parser__skip_spaces :: proc(instance: Parser) -> (consumed: bool) {
	for {
		tok := parser__get_token(instance)
		(tok.kind == .Space || tok.kind == .Newline) or_break
		parser__advance(instance)
		consumed = true
	}

	return consumed
}

parser__import :: proc(instance: Parser) -> (is_import, consumed: bool) {
  instance.can_import or_return
  tok := parser__get_token(instance)
  if tok.kind == .Apparition && tok.content == "import" {
    parser__advance(instance)
    site := instance.site

    inner_instance := instance
    inner_instance.codec = nil

    kind := parser__begin_apparition(instance, &inner_instance, tok)
    parser__skip_spaces(instance)
    path, path_ok := parser__parse_contiguous_text(instance)
    parser__skip_spaces(instance)
    parser__end_apparition(inner_instance, kind, tok)

    if !path_ok {
      site__errorf(site, tok, "Import path not specified")
      return
    }

    is_import = true
    site__frame(site, instance.scratch)
    allocator := site__alloc(site, .Stack)

    dir_path := Path__Input(slashpath.dir(string(instance.path), allocator))
    file_path := site__resolve(site, dir_path, Path(path))

    full_path := site__absolute(site, site.content_root, file_path, .Stack)
    bytes, err := os.read_entire_file_from_path(string(full_path), allocator)
    if err != nil {
      site__errorf(site, file_path, "Failed to read import: %v", err)
      return
    }

    site.statistics.files_read += 1
    tokens := parser__lex(site, file_path, string(bytes)) or_return

    file_instance := instance
    file_instance.tokens           = tokens
    file_instance.token            = new(uint, allocator)
    file_instance.ok               = new_clone(true, allocator)
    file_instance.in_paragraph     = false
    file_instance.indentation      = 0
    file_instance.surrounding_kind = .Indented

    consumed = codec__eval_instance(file_instance)
    instance.ok^ &&= file_instance.ok^
    if !file_instance.ok^ do return

    tok := parser__get_token(file_instance)
    if tok.kind != .Eof {
      site__errorf(site, tok, "Unexpected token %v.", tok)
    }

    return
  }

  return
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
      mem__same_layout(instance.codec.type, string),
			"Expected codec of type string. Got %v instead.",
			instance.codec.type,
		)

    if ok, consumed := parser__import(instance); ok do return consumed
    site__frame(instance.site, instance.scratch)
    str := parser__parse_contiguous_text(instance) or_return

    if !instance.scratch {
      clone, err := strings.clone(str, site__alloc(instance.site))
      log.assert(err == nil)
      str = clone
    }

		mem.copy(instance.output, &str, size_of(string))
		return true
	case Codec__Raw:
		log.assertf(
			instance.codec.type == string,
			"Expected codec of type string. Got %v instead.",
			instance.codec.type,
		)

    if ok, consumed := parser__import(instance); ok do return consumed
    site__frame(instance.site)

    forever_alloc := site__alloc(instance.site, .Forever)
    temp_alloc    := site__alloc(instance.site, .Stack)
    lines: Exparr(string)
    lines.allocator = temp_alloc

    builder: strings.Builder
    strings.builder_init_none(&builder, temp_alloc)

    should_continue := true
    for should_continue {
      for {
        tok := parser__get_token(instance, skip_comments = false)

        if tok.kind == .None do should_continue = false
        switch instance.surrounding_kind {
        case .Indented:
        case .Bracketed:
          if tok.kind == .RCurly {
            should_continue = false
          }
        case .Delimited:
          if tok.kind == instance.surrounding_delim_closer {
            should_continue = false
          }
        }

        if !should_continue do break

        parser__advance(instance)
        if tok.kind == .Newline do break

        #partial switch tok.kind {
        case .Apparition: fmt.sbprintf(&builder, "\\%v", tok.content)
        case: fmt.sbprint(&builder, tok.content)
        }
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
    size := reflect.size_of_typeid(instance.codec.type)
		mem.copy(instance.output, inner.value, size)
		return true
	case Codec__Token:
		tok := parser__get_token(instance)
		if tok.kind != inner.kind do return false
		parser__advance(instance)
    size := reflect.size_of_typeid(instance.codec.type)
		mem.copy(instance.output, inner.value, size)
		return true
	case Codec__Focus:
		site__frame(instance.site, instance.scratch)
    defer site__update_stack_stats(instance.site)

    inner_alloc := site__alloc(
      instance.site,
      .Stack if instance.scratch else .Forever
    )

    temp_alloc := site__alloc(instance.site, .Stack)
		inner_output := mem__reflect__new(inner.inner.type, temp_alloc)
    kit := Lens_Kit {
      outer           = instance.output,
      inner           = inner_output,
      outer_codec     = instance.codec,
      inner_codec     = inner.inner,
      user_data       = inner.user_data,
      document        = instance.document,
      mode            = .Project,
      allocator       = inner_alloc,
      temp_allocator  = temp_alloc,
      surrounded_at   = instance.surrounded_at,
      error_allocator = site__alloc(instance.site, .Forever),
      errors          = { allocator = temp_alloc },
    }

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
    if ok, consumed := parser__import(instance); ok do return consumed

    tok := instance.token^
		for &codec in inner {
			inner_instance := instance
			inner_instance.codec = &codec
      inner_instance.can_import = false

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
		make_completion_state(&inner_instance)

    kind := parser__begin_apparition(instance, &inner_instance, tok)
		codec__eval_instance(inner_instance)
    parser__end_apparition(inner_instance, kind, tok)
    codec__check_flags(tok, inner_instance)

		return true
	case Codec__Leaded:
		tok := parser__get_token(instance)
		if tok.kind != inner.by do return false
		parser__advance(instance)

		site__frame(instance.site, instance.scratch)

		inner_instance := instance
		inner_instance.codec = inner.inner
		make_completion_state(&inner_instance)

    kind := parser__begin_apparition(instance, &inner_instance, tok)
		codec__eval_instance(inner_instance)
    parser__end_apparition(inner_instance, kind, tok)
    codec__check_flags(tok, inner_instance)

		return true
	case Codec__Delimited:
		tok := parser__get_token(instance)
		if tok.kind != inner.opener do return false
    if instance.surrounding_delim_closer == inner.opener do return false
		parser__advance(instance)

		site__frame(instance.site, instance.scratch)

		inner_instance := instance
		inner_instance.codec = inner.inner
		make_completion_state(&inner_instance)

    inner_instance.in_paragraph = false
    inner_instance.can_import   = true
    inner_instance.surrounded_at = tok.from
    inner_instance.surrounding_kind = .Delimited
    inner_instance.surrounding_delim_closer = inner.closer

		codec__eval_instance(inner_instance)
    parser__end_apparition(inner_instance, .Delimited, tok)
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
    inner_instance.can_import = false

		tok := instance.token^
		for {
		  defer tok = instance.token^

      if ok, consumed_now := parser__import(instance); ok {
        consumed ||= consumed_now
        continue
      }

      consumed_now := codec__eval_instance(inner_instance)
      (consumed_now || instance.token^ > tok) or_break
      consumed ||= consumed_now
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
// }}}
// {{{ Flag checking
@(private = "package")
codec__check_flags :: proc(loc: Error_Location, instance: Parser) {
  inner_instance := instance
  switch inner in instance.codec.data {
  case Codec__Space, Codec__Constant, Codec__Token, Codec__Text, Codec__Raw,
       Codec__At, Codec__Leaded, Codec__Delimited, nil:
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
// {{{ Parsing entrypoint
@(private = "package")
parser__eval :: proc(
  site: ^Site, codec: ^Codec, path: Path__Input, source: string, output: rawptr,
) -> (ok: bool)  {
  site__frame(site)

  tokens := parser__lex(site, path, source) or_return
	instance := Parser {
    site       = site,
    path       = path,
		codec      = codec,
		output     = output,
    tokens     = tokens,
    can_import = true,
    // We could keep these on the proper stack but idrc
    token      = new(uint, site__alloc(site, .Stack)),
    ok         = new_clone(true, site__alloc(site, .Stack)),
	}

	make_completion_state(&instance)

	_ = codec__eval_instance(instance)
  if !instance.ok^ do return

  tok := parser__get_token(instance)
  if tok.kind != .Eof {
    site__errorf(site, tok, "Unexpected token %v.", tok)
    return
  }

  codec__check_flags(path, instance)
  if !instance.ok^ do return

  return true
}
// }}}
