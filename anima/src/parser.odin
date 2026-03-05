#+private file
package anima

import "core:fmt"
import "core:log"
import "core:mem"
import "core:mem/virtual"
import "core:reflect"

// {{{ The parser type
Surrounding_Apparition :: struct {
	indentation: uint,
	kind:        enum {
		Indented,
		Bracketed,
		Ambient,
	},
}

Indented_Token :: struct {
	using token: Token,
	indentation: uint,
}

// This is technically wasting some memory by storing the file pointer twice and
// whatnot, but it does not matter in the grand scheme of things.
@(private = "package")
Source_Range :: [2]Source_Loc

@(private = "package")
Error_Location :: union { Token, ^File, Source_Range, Path__Absolute }

@(private = "package")
Parsing_Error :: struct {
  loc: Error_Location,
	msg: string,
}

@(private = "package")
Parser :: struct {
	internal_arena:     virtual.Arena, // Every other piece of internal data
	codec_output_stack: virtual.Arena, // Temporary data for use by Codec__Focus
	codec_state_stack:  virtual.Arena, // Temporary frames forcompletion tracking
	error_arena:        virtual.Arena, // Error messages
	output_arena:       virtual.Arena, // Output data
	tokens:             Exparr(Indented_Token, 10),
	token:              uint, // The index of the current token
	stack:              Exparr(Surrounding_Apparition, 4),
	errors:             Exparr(Parsing_Error),
	statistics:         ^Statistics,
}

@(private = "package")
parser__make :: proc(parser: ^Parser, statistics: ^Statistics) {
  log.assert(mem__iz(parser^))

	parser.statistics = statistics
	err := virtual.arena_init_static(&parser.error_arena)
	log.assert(err == nil)
	err = virtual.arena_init_static(&parser.codec_output_stack)
	log.assert(err == nil)
	err = virtual.arena_init_static(&parser.codec_state_stack)
	log.assert(err == nil)
	err = virtual.arena_init_static(&parser.internal_arena)
	log.assert(err == nil)
	err = virtual.arena_init_static(&parser.output_arena)
	log.assert(err == nil)

	parser.tokens.allocator = virtual.arena_allocator(&parser.internal_arena)
	parser.stack.allocator = virtual.arena_allocator(&parser.internal_arena)
	parser.errors.allocator = virtual.arena_allocator(&parser.error_arena)

	exparr__push(&parser.stack, Surrounding_Apparition{})
}

@(private = "package")
parser__destroy :: proc(parser: ^Parser) {
	virtual.arena_destroy(&parser.codec_output_stack)
	virtual.arena_destroy(&parser.codec_state_stack)
	virtual.arena_destroy(&parser.internal_arena)
	virtual.arena_destroy(&parser.error_arena)
	virtual.arena_destroy(&parser.output_arena)
}

@(private = "package")
parser__clear :: proc(parser: ^Parser) {
  exparr__clear(&parser.errors)
  exparr__clear(&parser.tokens)
  exparr__clear(&parser.stack)
  virtual.arena_free_all(&parser.codec_output_stack)
  virtual.arena_free_all(&parser.codec_state_stack)

	exparr__push(&parser.stack, Surrounding_Apparition{})
  parser.token = 0
}

parser__error :: proc(parser: ^Parser, loc: Error_Location, msg: string) {
	exparr__push(&parser.errors, Parsing_Error{loc, msg})
}

parser__errorf :: proc(
  parser: ^Parser, loc: Error_Location, format: string, args: ..any
) {
	allocator := virtual.arena_allocator(&parser.error_arena)
	msg := fmt.aprintf(format, ..args, allocator = allocator)
	parser__error(parser, loc, msg)
}
// }}}
// {{{ Lexing
@(private = "package")
parser__lex :: proc(parser: ^Parser, file: ^File) -> (ok: bool) {
	log.assert(parser.tokens.len == 0, "Cannot lex inside a non-clean parser")
  parser.token = 0

	lexer := lexer__make(file, &parser.output_arena) or_return

	for {
    tok: Token
		tok, ok = tokenize(&lexer)

		if !ok {
			tok.from = lexer.error.pos
			parser__error(parser, tok, lexer.error.msg)
			return false
		}

		itok := Indented_Token {
			token       = tok,
			indentation = tok.from.col,
		}

		exparr__push(&parser.tokens, itok)

		if tok.kind == .Eof do break
	}

	current_indentation: uint = 0
	for i := parser.tokens.len - 1; int(i) >= 0; i -= 1 {
		tok := exparr__get(parser.tokens, i)
		if tok.kind == .Space || tok.kind == .Newline {
			tok.indentation = current_indentation
		} else {
			current_indentation = tok.indentation
		}
	}

	parser.statistics.tokens += parser.tokens.len

	return true
}
// }}}
// {{{ Token handling
parser__get_pos :: proc(parser: Parser) -> Source_Loc {
  return exparr__get(parser.tokens, parser.token).from
}

parser__advance :: proc(parser: ^Parser) {
	parser.token += 1
}

parser__get_token :: proc(instance: Codec_Instance) -> (tok: Token) {
	itok := exparr__get(instance.parser.tokens, instance.parser.token)^
	indentation := exparr__last(instance.parser.stack).indentation

	if itok.indentation <= indentation {
		return {from = tok.from}
	}

	// Paragraphs get terminated by consecutive newlines.
	if instance.in_paragraph && itok.kind == .Newline {
		offset: uint = 1
		for {
      index     := instance.parser.token + offset
			next_itok := exparr__get(instance.parser.tokens, index)^

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
// If codecs are templates for parsers, a codec instance is the actual parser,
// together with its state.
Codec_Instance :: struct {
	parser:       ^Parser,
	codec:        ^Codec,
	output:       rawptr,
	in_paragraph: bool,
	document:     rawptr, // Top-level context any function can access

	// The capacity for this list is computed at the start of the block, and its
	// allocator is set to the panic allocator. We could instead store this as a
	// capacity+slice combination, but the extra pointers required to store the
	// panic allocator doesn't matter in the grand scheme of things.
	//
	// A more efficient representation would use a bitset, but that's not worth
	// spending time on right now.
	completed_codecs: ^[dynamic]^Codec,
}

// Count the number of codecs in the current block that care about keeping track
// of whether they've been completed or not.
codec__count_completable :: proc(instance: Codec_Instance) -> uint {
	switch inner in instance.codec.data {
	case Codec__Space, Codec__Constant, Codec__Text, Codec__At, nil:
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

codec__is_completed :: proc(instance: Codec_Instance) -> bool {
	for completed in instance.completed_codecs {
		if completed == instance.codec do return true
	}

	return false
}

codec__mark_completed :: proc(instance: Codec_Instance, could_be_completed := true) {
	if could_be_completed && codec__is_completed(instance) do return
	append_elem(instance.completed_codecs, instance.codec)
}

codec__make_completed_state :: proc(instance: Codec_Instance) -> ^[dynamic](^Codec) {
	allocator := virtual.arena_allocator(&instance.parser.codec_state_stack)

	completed_codecs := make_dynamic_array_len_cap(
		[dynamic](^Codec),
		0,
		codec__count_completable(instance),
		allocator,
	)

	completed_codecs.allocator = mem.panic_allocator()
	return new_clone(completed_codecs, allocator)
}

parser__skip_spaces :: proc(instance: Codec_Instance) -> (consumed: bool) {
	for {
		tok := parser__get_token(instance)
		(tok.kind == .Space || tok.kind == .Newline) or_break
		parser__advance(instance.parser)
		consumed = true
	}

	return consumed
}

codec__eval_instance :: proc(instance: Codec_Instance) -> (consumed: bool) {
	instance.parser.statistics.codec_evaluations += 1
	switch inner in instance.codec.data {
	case Codec__Space:
		return parser__skip_spaces(instance)
	case Codec__Text:
		log.assertf(
			instance.codec.type == string,
			"Expected codec of type string. Got %v instead.",
			instance.codec.type,
		)

		tok := parser__get_token(instance)
		if tok.kind != .Word && tok.kind != .Bang do return false
		parser__advance(instance.parser)
		mem.copy(instance.output, &tok.content, size_of(string))
		return true
	case Codec__Constant:
		tok := parser__get_token(instance)
		if tok.kind != .Apparition do return false
		if tok.content != inner.name do return false
		parser__advance(instance.parser)
		mem.copy(instance.output, inner.value, reflect.size_of_typeid(instance.codec.type))
		return true
	case Codec__Focus:
		temp := virtual.arena_temp_begin(&instance.parser.codec_output_stack)
		defer virtual.arena_temp_end(temp)
		allocator := virtual.arena_allocator(&instance.parser.codec_output_stack)

		inner_output := mem__reflected_new(inner.inner.type, allocator)
    kit := Lens_Kit {
      outer           = instance.output,
      inner           = inner_output,
      user_data       = inner.user_data,
      document        = instance.document,
      mode            = .Project,
      allocator       = virtual.arena_allocator(&instance.parser.output_arena),
      temp_allocator  = allocator,
      error_allocator = virtual.arena_allocator(&instance.parser.error_arena),
    }

    kit.errors.allocator = kit.temp_allocator

		inner.lens(&kit)
    log.assert(!kit.consumed, ".Project cannot be marked as consuming")

    pos__pre := parser__get_pos(instance.parser^)
    if kit.errors.len == 0 {
      inner_instance := instance
      inner_instance.codec = inner.inner
      inner_instance.output = inner_output
      inner_instance.document = kit.document

      consumed = codec__eval_instance(inner_instance)
      if consumed {
        kit.mode = .Inject
        kit.consumed = consumed
        inner.lens(&kit)
      }

      if kit.errors.len == 0 do return kit.consumed
    }

    pos__post := parser__get_pos(instance.parser^)
    loc := Source_Range{pos__pre, pos__post}
    for i in 0..<kit.errors.len {
      msg := exparr__get(kit.errors, i)^
      parser__error(instance.parser, loc, msg)
    }

    return consumed
	case Codec__Sum:
    tok := instance.parser.token
		for &codec in inner {
			inner_instance := instance
			inner_instance.codec = &codec

			consumed := codec__eval_instance(inner_instance)
      if consumed || instance.parser.token > tok do return consumed
		}

		return false
	case Codec__At:
		tok := parser__get_token(instance)
		if tok.kind != .Apparition do return false
		if tok.content != inner.name do return false
		parser__advance(instance.parser)

		temp := virtual.arena_temp_begin(&instance.parser.codec_state_stack)
		defer virtual.arena_temp_end(temp)

		elem: Surrounding_Apparition = {}

		next_tok := parser__get_token(instance)
		#partial switch next_tok.kind {
		case .LCurly:
			parser__advance(instance.parser)
			elem.kind = .Bracketed
			elem.indentation = exparr__last(instance.parser.stack).indentation
		case .Bang:
			parser__advance(instance.parser)
			elem.kind = .Ambient
			elem.indentation = exparr__last(instance.parser.stack).indentation
		case:
			elem.kind = .Indented
			elem.indentation = tok.from.col
		}

		inner_instance := instance
		inner_instance.codec = inner.inner
		inner_instance.completed_codecs = codec__make_completed_state(inner_instance)
		inner_instance.in_paragraph = false

		exparr__push(&instance.parser.stack, elem)
		defer exparr__pop(&instance.parser.stack)

		parser__skip_spaces(instance)
		codec__eval_instance(inner_instance)
		last_tok := parser__get_token(instance)

		if elem.kind == .Bracketed {
			if last_tok.kind == .RCurly {
				parser__advance(instance.parser)
			} else {
				parser__errorf(
					instance.parser,
					last_tok,
					"I don't know how to parse this token. Common causes:\n" +
          "- missing closing brackets\n" +
          "- trying to use a unique apparition more than once per scope",
					next_tok.from,
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

		tok := instance.parser.token
		for {
      consumed_now := codec__eval_instance(inner_instance)
		  (consumed_now || instance.parser.token > tok) or_break
      consumed ||= consumed_now
		  tok = instance.parser.token
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
codec__check_flags :: proc(loc: Error_Location, instance: Codec_Instance) {
  inner_instance := instance
  switch inner in instance.codec.data {
	case Codec__Space, Codec__Constant, Codec__Text, Codec__At, nil:
  case Codec__Focus: 
    inner_instance.codec = inner.inner
    codec__check_flags(loc, inner_instance)
  case Codec__Loop: 
    inner_instance.codec = cast(^Codec)inner
    codec__check_flags(loc, inner_instance)
  case Codec__Paragraph: 
    inner_instance.codec = cast(^Codec)inner
    codec__check_flags(loc, inner_instance)
  case Codec__Sum: 
    for &codec in inner {
      inner_instance.codec = &codec
      codec__check_flags(loc, inner_instance)
    }
  case Codec__Tracked:
    inner_instance.codec = inner.inner
    if .Required in inner.flags && !codec__is_completed(inner_instance) {
      parser__errorf(
        instance.parser,
        loc,
        "Argument \"%v\" is required in this scope.",
        inner.name,
      )
    } else {
      codec__check_flags(loc, inner_instance)
    }
  }
}

@(private = "package")
parser__eval :: proc(
  parser: ^Parser, codec: ^Codec, document: rawptr = nil
) -> (output: rawptr) {
	temp_output := virtual.arena_temp_begin(&parser.output_arena)
	defer virtual.arena_temp_ignore(temp_output)
	output_allocator := virtual.arena_allocator(&parser.output_arena)

	temp_state := virtual.arena_temp_begin(&parser.codec_state_stack)
	defer virtual.arena_temp_end(temp_state)

	output = mem__reflected_new(codec.type, output_allocator)
	instance := Codec_Instance {
		parser   = parser,
		codec    = codec,
		output   = output,
    document = document
	}

	instance.completed_codecs = codec__make_completed_state(instance)

  file := parser__get_pos(parser^).file
	_ = codec__eval_instance(instance)

	tok := exparr__get(parser.tokens, parser.token)
  if tok.kind != .Eof {
    parser__error(parser, tok.token, "Unexpected token. Expected end of file.")
  }

  if parser.errors.len == 0 do codec__check_flags(file, instance)

	return output
}
// }}}
