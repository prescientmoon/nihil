package anima

import "base:runtime"
import "core:fmt"
import "core:strings"
import "core:time"

// {{{ The parser type
Parser_Stack_Elem :: struct {
	indentation: uint,
	curly:       bool,
}

Parser :: struct {
	allocator: runtime.Allocator,
	lexer:     Lexer,
	tok:       Token,
	stack:     Exparr(Parser_Stack_Elem, 3),
	error:     struct {
		tok: Token,
		msg: string,
	},
}

mk_parser :: proc(source: string, allocator := context.allocator) -> (parser: Parser, ok: bool) {
	lexer, lexer_ok := mk_lexer(source, allocator)
	tok, first_tok_ok := tokenize(&lexer)

	parser = {
		allocator = allocator,
		lexer = lexer,
		tok = tok,
		stack = {allocator = allocator},
	}

	exparr_push(&parser.stack, Parser_Stack_Elem{})
	return parser, lexer_ok && first_tok_ok
}
// }}}
// {{{ Lexer helpers
advance_token :: proc(parser: ^Parser) -> (ok: bool) {
	parser.tok = tokenize(&parser.lexer) or_return
	return true
}

get_token :: proc(parser: ^Parser, skip_ws := false) -> (tok: Token, ok: bool) {
	stack_elem := exparr_last(&parser.stack)
	for {
		if skip_ws && (parser.tok.kind == .Newline || parser.tok.kind == .Space) {
			advance_token(parser) or_return
		} else if parser.tok.kind == .Apparition && parser.tok.content == "--" {
			apparition := parser.tok
			advance_token(parser) or_return

			apparition_arg_begin(parser, apparition) or_return

			stack_elem := exparr_last(&parser.stack)
			for {
				tok := get_token(parser, skip_ws = true) or_return
				if tok.kind == .None ||
				   tok.kind == .Eof ||
				   stack_elem.curly && tok.kind == .RCurly {
					break
				} else {
					advance_token(parser) or_return
				}
			}

			apparition_arg_end(parser) or_return
		} else {
			break
		}
	}

	return get_indented_token(parser)
}

get_indented_token :: proc(parser: ^Parser) -> (tok: Token, ok: bool) {
	indentation := exparr_last(&parser.stack).indentation

	if parser.tok.from.col <= indentation &&
	   parser.tok.kind != .Newline &&
	   parser.tok.kind != .Space {
		return {from = parser.tok.from}, true
	} else {
		return parser.tok, true
	}
}

expect_token :: proc(
	parser: ^Parser,
	kind: Token_Kind,
	content := "",
) -> (
	tok: Token,
	found: bool,
	ok: bool,
) {
	tok = get_token(parser) or_return
	found = tok.kind == kind && (content == "" || tok.content == content)
	if found do advance_token(parser) or_return
	return tok, found, true
}
// }}}
// {{{ Apparitions
apparition_arg_begin :: proc(parser: ^Parser, apparition: Token) -> (ok: bool) {
	assert(apparition.kind == .Apparition)

	tok := get_token(parser, skip_ws = true) or_return

	if tok.kind == .LCurly {
		elem := Parser_Stack_Elem {
			curly       = true,
			indentation = 0,
		}

		advance_token(parser)
		exparr_push(&parser.stack, elem)
	} else {
		elem := Parser_Stack_Elem {
			curly       = false,
			indentation = apparition.from.col,
		}

		exparr_push(&parser.stack, elem)
	}

	return true
}

apparition_arg_end :: proc(parser: ^Parser) -> (ok: bool) {
	elem := exparr_pop(&parser.stack)

	if elem.curly {
		tok := get_token(parser, skip_ws = true) or_return

		if tok.kind != .RCurly {
			parser.error = {
				tok = tok,
				msg = "Expected '}'",
			}

			return false
		}

		advance_token(parser) or_return
	}

	return true
}
// }}}
// {{{ Strings
String_Parser :: struct {
	size:     uint,
	segments: Exparr(Token, 3),
}

string_parser_mk :: proc(parser: ^Parser) -> String_Parser {
	return {size = 0, segments = {allocator = parser.allocator}}
}

string_parser_end :: proc(parser: ^Parser, sp: ^String_Parser) -> string {
	// Remove spirious trailing spaces
	if sp.segments.len > 0 && exparr_last(&sp.segments).kind == .Space {
		exparr_pop(&sp.segments)
	}

	// Allocate builder for the output string. Should never grow past the given
	// size!
	builder := strings.builder_make_len_cap(0, int(sp.size), parser.allocator)

	// Sanity check: attempting to re-allocate the buffer will cause a panic!
	builder.buf.allocator = runtime.panic_allocator()

	for i in 0 ..< sp.segments.len {
		tok := exparr_get(&sp.segments, i)
		strings.write_string(&builder, tok.content)
	}

	return strings.to_string(builder)
}

string_parser_run :: proc(parser: ^Parser, sp: ^String_Parser) -> (consumed: bool, ok: bool) {
	tok := get_token(parser) or_return
	#partial switch tok.kind {
	case .Word:
		advance_token(parser) or_return
		exparr_push(&sp.segments, tok)
		sp.size += len(tok.content)
	case .Space, .Newline:
		advance_token(parser) or_return

		if sp.segments.len == 0 || exparr_last(&sp.segments).kind == .Space {
			return true, true
		}

		exparr_push(&sp.segments, Token{content = " ", kind = .Space, from = tok.from})
		sp.size += 1
	case:
		return false, true
	}

	return true, true
}
// }}}
// {{{ Inline parsing
parse_inline :: proc(parser: ^Parser) -> (res: Inline_Markup, ok: bool) {
	tok := get_token(parser) or_return
	switch tok.kind {
	case .Space, .Newline:
		advance_token(parser)
		res = {
			kind = .Space,
		}
	case .Word:
		advance_token(parser)
		res = {
			kind = .Text,
			raw  = tok.content,
		}
	case .None, .LCurly, .RCurly, .Eof: // Do nothing
	case .Apparition:
		switch tok.content {
		case "*", "_", "~", "`", "\"":
			advance_token(parser)
			switch tok.content {
			case "*":
				res.kind = .Strong
			case "_":
				res.kind = .Emph
			case "~":
				res.kind = .Strikethrough
			case "`":
				res.kind = .Mono
			case "\"":
				res.kind = .Quote
			}

			apparition_arg_begin(parser, tok) or_return
			res.inner = new_clone(parse_inlines(parser) or_return, parser.allocator)
			apparition_arg_end(parser) or_return
		case "$", "icon", "fn":
			advance_token(parser)
			switch tok.content {
			case "$":
				res.kind = .LaTeX
			case "icon":
				res.kind = .Icon
			case "fn":
				res.kind = .Fn
			}

			apparition_arg_begin(parser, tok) or_return
			sp := string_parser_mk(parser)
			for do (string_parser_run(parser, &sp) or_return) or_break
			res.raw = string_parser_end(parser, &sp)
			apparition_arg_end(parser) or_return
		case "link":
			advance_token(parser)
			res.kind = .Link

			apparition_arg_begin(parser, tok) or_return
			sp := string_parser_mk(parser)
			for {
				if tok, ok := expect_token(parser, .Apparition, "label") or_return; ok {
					if res.link.label == nil {
						apparition_arg_begin(parser, tok) or_return
						inlines := parse_inlines(parser) or_return
						apparition_arg_end(parser) or_return
						res.link.label = new_clone(inlines, parser.allocator)
						continue
					} else {
						parser.error = {tok, "Link label specified twice"}
						return res, false
					}
				}

				(string_parser_run(parser, &sp) or_return) or_break
			}

			apparition_arg_end(parser) or_return
			res.link.id = string_parser_end(parser, &sp)
		case "date", "datetime":
			advance_token(parser)
			switch tok.content {
			case "date":
				res.kind = .Date
			case "datetime":
				res.kind = .Datetime
			}

			apparition_arg_begin(parser, tok) or_return
			sp := string_parser_mk(parser)
			for {
				if tok, ok := expect_token(parser, .Apparition, "compact") or_return; ok {
					res.time.compact = true
					continue
				}

				(string_parser_run(parser, &sp) or_return) or_break
			}
			apparition_arg_end(parser) or_return
			as_string := string_parser_end(parser, &sp)

			datetime, datetime_consumed := time.iso8601_to_time_utc(as_string)
			if datetime_consumed > 0 do res.time.time = datetime
			else {
				// Try to tack an empty timestamp at the end
				as_date_string := fmt.aprintf("%vT00:00:00+00:00", as_string, allocator = parser.allocator)
				date, date_consumed := time.iso8601_to_time_utc(as_date_string)
				if date_consumed > 0 do res.time.time = date
				else {
					parser.error = {tok, "Invalid date(time)"}
					return res, false
				}
			}
		case "...":
			advance_token(parser)
			res.kind = .Ellipsis
		}
	}

	return res, true
}

parse_inlines :: proc(parser: ^Parser) -> (res: Inline_Markup, ok: bool) {
	ip := inline_parser_mk(parser)
	for do (inline_parser_run(parser, &ip) or_return) or_break
	return inline_parser_end(parser, &ip), true
}

Inline_Parser :: struct {
	elements: Exparr(Inline_Markup, 3),
}

inline_parser_mk :: proc(parser: ^Parser) -> (res: Inline_Parser) {
	res.elements.allocator = parser.allocator
	return res
}

inline_parser_end :: proc(parser: ^Parser, ip: ^Inline_Parser) -> (res: Inline_Markup) {
	// Remove spirious trailing spaces
	if ip.elements.len > 0 && exparr_last(&ip.elements).kind == .Space {
		exparr_pop(&ip.elements)
	}

	if ip.elements.len == 1 {
		res = exparr_last(&ip.elements)^
	} else {
		res = {
			kind = .Many,
			many = ip.elements,
		}
	}

	return res
}

inline_parser_run :: proc(parser: ^Parser, ip: ^Inline_Parser) -> (consumed: bool, ok: bool) {
	elem := parse_inline(parser) or_return
	#partial switch elem.kind {
	case .None:
		return false, true
	case .Space:
		if ip.elements.len == 0 || exparr_last(&ip.elements).kind == .Space {
			return true, true
		}
	}

	exparr_push(&ip.elements, elem)
	return true, true
}

// }}}
