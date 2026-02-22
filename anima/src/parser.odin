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
	lexer:              Lexer,
	tokens:             Exparr(Indented_Token, 3),
	stack:              Exparr(Surrounding_Apparition, 3),
	errors:             Exparr(Parsing_Error),
}

@(private = "package")
mk_parser :: proc(source: string, parser: ^Parser) -> (ok: bool) {
	lexer, lexer_ok := mk_lexer(source)
	parser.lexer = lexer

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

	exparr__push(&parser.stack, Surrounding_Apparition{})
	return lexer_ok
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
advance_token :: proc(parser: ^Parser) {
	exparr__pop(&parser.tokens)
}

parser__error :: proc(parser: ^Parser, tok: Token, msg: string) {
	exparr__push(&parser.errors, Parsing_Error{tok, msg})
}

parser__errorf :: proc(parser: ^Parser, tok: Token, format: string, args: ..any) {
	allocator := virtual.arena_allocator(&parser.error_arena)
	msg := fmt.aprintf(format, ..args, allocator = allocator)
	parser__error(parser, tok, msg)
}

parser__get_token :: proc(parser: ^Parser) -> (tok: Token, ok: bool) {
	// When we run out of tokens, we run the lexer until we hit a non-whitespace
	// character. These tokens will not get thrown out, but will get adjusted to
	// match the indentation of the following token.
	if parser.tokens.len == 0 {
		for {
			tok, ok := tokenize(&parser.lexer)

			if !ok {
				tok.from = parser.lexer.error.pos
				parser__error(parser, tok, parser.lexer.error.msg)
			}

			itok := Indented_Token {
				token       = tok,
				indentation = tok.from.col,
			}

			exparr__push(&parser.tokens, itok)
			if tok.kind != .Space && tok.kind != .Newline do break
		}

		last_indent := exparr__last(parser.tokens).indentation
		exparr__reverse(parser.tokens)

		// Indent spaces/newlines as much as the following token
		for i in 1 ..< parser.tokens.len {
			exparr__get(parser.tokens, i).indentation = last_indent
		}
	}

	// Sanity check
	assert(parser.tokens.len > 0)

	indentation := exparr__last(parser.stack).indentation
	itok := exparr__last(parser.tokens)

	if itok.indentation <= indentation {
		return {from = itok.from}, true
	} else {
		return itok.token, true
	}
}
// }}}
// {{{ Codec evaluation
// If codecs are templates for parsers, a codec instance is the actual parser,
// together with its state.
Codec_Instance :: struct {
	parser:           ^Parser,
	codec:            ^Codec,
	output:           rawptr,

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

codec__eval_instance :: proc(instance: Codec_Instance) -> (consumed: bool, ok: bool) {
	switch inner in instance.codec.data {
	case Codec__Space:
		for {
			tok := parser__get_token(instance.parser) or_return
			(tok.kind == .Space) or_break
			advance_token(instance.parser)
			consumed = true
		}

		return consumed, true
	case Codec__Text:
		log.assertf(
			instance.codec.type == string,
			"Expected codec of type string. Got %v instead.",
			instance.codec.type,
		)

		tok := parser__get_token(instance.parser) or_return
		if tok.kind != .Word && tok.kind != .Bang && tok.kind != .Colon do return false, true
		advance_token(instance.parser)
		mem.copy(instance.output, &tok.content, size_of(string))
		return true, true
	case Codec__Constant:
		tok := parser__get_token(instance.parser) or_return
		if tok.kind != .Apparition do return false, true
		if tok.content != inner.name do return false, true
		advance_token(instance.parser)
		mem.copy(instance.output, inner.value, reflect.size_of_typeid(instance.codec.type))
		return true, true
	case Codec__Focus:
		temp := virtual.arena_temp_begin(&instance.parser.codec_output_stack)
		defer virtual.arena_temp_end(temp)
		allocator := virtual.arena_allocator(&instance.parser.codec_output_stack)

		// project & inject can reference this
		context.allocator = virtual.arena_allocator(&instance.parser.output_arena)

		inner_output := dynamic_new(inner.inner.type, allocator)
		inner.project(instance.output, inner_output)

		inner_instance := instance
		inner_instance.codec = inner.inner
		inner_instance.output = inner_output
		consumed := codec__eval_instance(inner_instance) or_return
		if consumed do inner.inject(instance.output, inner_output)

		return consumed, true
	case Codec__Sum:
		for &codec in inner {
			inner_instance := instance
			inner_instance.codec = &codec
			consumed := codec__eval_instance(inner_instance) or_return
			if consumed do return true, true
		}

		return false, true
	case Codec__At:
		tok := parser__get_token(instance.parser) or_return
		if tok.kind != .Apparition do return false, true
		if tok.content != inner.name do return false, true
		advance_token(instance.parser)

		temp := virtual.arena_temp_begin(&instance.parser.codec_state_stack)
		defer virtual.arena_temp_end(temp)
		allocator := virtual.arena_allocator(&instance.parser.codec_state_stack)

		// TODO: handle proper argument syntax here
		inner_instance := instance
		inner_instance.codec = inner.inner
		inner_instance.completed_codecs = codec__make_completed_state(inner_instance)

		consumed = codec__eval_instance(inner_instance) or_return
		// TODO: check "required" flags
		return consumed, true
	case Codec__Tracked:
		inner_instance := instance
		inner_instance.codec = inner.inner

		is_completed := codec__is_completed(inner_instance)
		if is_completed && inner.unique do return false, true

		consumed = codec__eval_instance(inner_instance) or_return
		if !is_completed && consumed {
			codec__mark_completed(inner_instance, could_be_completed = false)
		}

		return consumed, true
	case Codec__Loop:
		inner_instance := instance
		inner_instance.codec = cast(^Codec)inner

		for codec__eval_instance(inner_instance) or_return {
			consumed = true
		}

		return consumed, true
	case nil:
		log.panic("Cannot evaluate the nil codec.")
	}

	log.panic("impossible", instance.codec)
}

@(private = "package")
codec__eval :: proc(parser: ^Parser, codec: ^Codec) -> (output: rawptr, ok: bool) {
	temp_output := virtual.arena_temp_begin(&parser.output_arena)
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
	consumed, eval_ok := codec__eval_instance(instance)
	if !eval_ok || !consumed {
		virtual.arena_temp_end(temp_output)
		return nil, eval_ok
	} else {
		virtual.arena_temp_ignore(temp_output)
		return output, true
	}
}
// }}}
