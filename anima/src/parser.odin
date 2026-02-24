#+private file
package anima

import "base:runtime"
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

Parsing_Error :: struct {
	tok: Token,
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
	parser.statistics = statistics
	err := virtual.arena_init_static(&parser.error_arena)
	assert(err == nil)
	err = virtual.arena_init_static(&parser.codec_output_stack)
	assert(err == nil)
	err = virtual.arena_init_static(&parser.codec_state_stack)
	assert(err == nil)
	err = virtual.arena_init_static(&parser.internal_arena)
	assert(err == nil)
	err = virtual.arena_init_static(&parser.output_arena)
	assert(err == nil)

	parser.tokens.allocator = virtual.arena_allocator(&parser.internal_arena)
	parser.stack.allocator = virtual.arena_allocator(&parser.internal_arena)
	parser.errors.allocator = virtual.arena_allocator(&parser.error_arena)

	exparr__push(&parser.stack, Surrounding_Apparition{})
}

@(private = "package")
parser__lex :: proc(parser: ^Parser, source: string) -> (ok: bool) {
	log.assert(parser.tokens.len == 0, "Cannot lex inside a non-clean parser")
	lexer := lexer__make(source, &parser.output_arena) or_return

	for {
		tok, ok := tokenize(&lexer)

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
// {{{ Memory helpers
// Similar to the standard library's "new", except the type of the allocation
// need not be known at compile time.
dynamic_new :: proc(type: typeid, allocator := context.allocator) -> rawptr {
	if ti := type_info_of(type); ti != nil {
		ptr, err := mem.alloc(size = ti.size, alignment = ti.align, allocator = allocator)
		assert(err == nil)
		return ptr
	}

	return nil
}
// }}}
// {{{ Lexer helpers
parser__advance :: proc(parser: ^Parser) {
	parser.token += 1
}

parser__error :: proc(parser: ^Parser, tok: Token, msg: string) {
	exparr__push(&parser.errors, Parsing_Error{tok, msg})
}

parser__errorf :: proc(parser: ^Parser, tok: Token, format: string, args: ..any) {
	allocator := virtual.arena_allocator(&parser.error_arena)
	msg := fmt.aprintf(format, ..args, allocator = allocator)
	parser__error(parser, tok, msg)
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
			next_itok := exparr__get(instance.parser.tokens, instance.parser.token + offset)^

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
	parser:           ^Parser,
	codec:            ^Codec,
	output:           rawptr,
	in_paragraph:     bool,

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

		// project & inject can reference these
		context.allocator = virtual.arena_allocator(&instance.parser.output_arena)
		context.user_ptr = inner.user_data

		inner_output := dynamic_new(inner.inner.type, allocator)
		inner.project(instance.output, inner_output)

		inner_instance := instance
		inner_instance.codec = inner.inner
		inner_instance.output = inner_output
		consumed := codec__eval_instance(inner_instance)
		if consumed do inner.inject(instance.output, inner_output)

		return consumed
	case Codec__Sum:
		for &codec in inner {
			inner_instance := instance
			inner_instance.codec = &codec
			consumed := codec__eval_instance(inner_instance)
			if consumed do return true
		}

		return false
	case Codec__At:
		tok := parser__get_token(instance)
		if tok.kind != .Apparition do return false
		if tok.content != inner.name do return false
		parser__advance(instance.parser)

		temp := virtual.arena_temp_begin(&instance.parser.codec_state_stack)
		defer virtual.arena_temp_end(temp)
		allocator := virtual.arena_allocator(&instance.parser.codec_state_stack)

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
					"Unexpected token. I was looking for a '}' to close out the token at %v.",
					next_tok.from,
				)
			}
		}

		// TODO: check "required" flags
		return true
	case Codec__Tracked:
		inner_instance := instance
		inner_instance.codec = inner.inner

		is_completed := codec__is_completed(inner_instance)
		if is_completed && inner.unique do return false

		consumed = codec__eval_instance(inner_instance)
		if !is_completed && consumed {
			codec__mark_completed(inner_instance, could_be_completed = false)
		}

		return consumed
	case Codec__Loop:
		inner_instance := instance
		inner_instance.codec = cast(^Codec)inner

		for codec__eval_instance(inner_instance) {
			consumed = true
		}

		return consumed
	case Codec__Paragraph:
		inner_instance := instance
		inner_instance.codec = cast(^Codec)inner
		inner_instance.in_paragraph = true

		consumed := codec__eval_instance(inner_instance)
		return consumed
	case nil:
		log.panic("Cannot evaluate the nil codec.")
	}

	log.panic("impossible", instance.codec)
}

@(private = "package")
codec__eval :: proc(parser: ^Parser, codec: ^Codec) -> (output: rawptr, ok: bool) {
	temp_output := virtual.arena_temp_begin(&parser.output_arena)
	defer virtual.arena_temp_ignore(temp_output)
	output_allocator := virtual.arena_allocator(&parser.output_arena)

	temp_state := virtual.arena_temp_begin(&parser.codec_state_stack)
	defer virtual.arena_temp_end(temp_state)

	output = dynamic_new(codec.type, output_allocator)
	instance := Codec_Instance {
		parser = parser,
		codec  = codec,
		output = output,
	}

	instance.completed_codecs = codec__make_completed_state(instance)

	// TODO: check "required" flags
	_ = codec__eval_instance(instance)
	return output, true
}
// }}}
