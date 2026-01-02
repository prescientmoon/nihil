package anima

import "base:runtime"
import "core:container/queue"
import "core:fmt"
import "core:mem"
import "core:strings"
import "core:time"

// {{{ The parser type
Parser_Stack_Elem :: struct {
	indentation: uint,
	curly:       bool,
}

Indented_Token :: struct {
	using token: Token,
	indentation: uint,
}

Parser :: struct {
	allocator: runtime.Allocator,
	lexer:     Lexer,
	tokens:    queue.Queue(Indented_Token),
	stack:     Exparr(Parser_Stack_Elem, 3),
	error:     struct {
		tok: Token,
		msg: string,
	},
}

mk_parser :: proc(source: string, allocator := context.allocator) -> (parser: Parser, ok: bool) {
	lexer, lexer_ok := mk_lexer(source, allocator)

	parser = {
		allocator = allocator,
		lexer = lexer,
		stack = {allocator = allocator},
	}

	queue.init(&parser.tokens, allocator = context.allocator)

	exparr_push(&parser.stack, Parser_Stack_Elem{})
	return parser, lexer_ok
}
// }}}
// {{{ Lexer helpers
advance_token :: proc(parser: ^Parser) {
	popped := queue.pop_front(&parser.tokens)
}

get_token :: proc(parser: ^Parser, skip_ws := false) -> (tok: Token, ok: bool) {
	stack_elem := exparr_last(&parser.stack)
	for {
		tok := get_indented_token(parser) or_return
		if skip_ws && (tok.kind == .Newline || tok.kind == .Space) {
			advance_token(parser)
		} else if tok.kind == .Apparition && tok.content == "--" {
			advance_token(parser)

			apparition_arg_begin(parser, tok) or_return
			stack_elem := exparr_last(&parser.stack)
			for {
				tok := get_token(parser, skip_ws = true) or_return
				if tok.kind == .None ||
				   tok.kind == .Eof ||
				   stack_elem.curly && tok.kind == .RCurly {
					break
				} else {
					advance_token(parser)
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
	// When we run out of tokens, we run the lexer until we hit a non-whitespace
	// character.
	if queue.len(parser.tokens) == 0 {
		for {
			tok := tokenize(&parser.lexer) or_return
			queue.push_back(
				&parser.tokens,
				Indented_Token{token = tok, indentation = tok.from.col},
			)
			if tok.kind != .Space && tok.kind != .Newline do break
		}

		// Indent spaces/newlines as much as the following token
		for i := queue.len(parser.tokens) - 2; i >= 0; i -= 1 {
			curr := queue.get_ptr(&parser.tokens, i)
			next := queue.get_ptr(&parser.tokens, i + 1)
			curr.indentation = next.indentation
		}
	}

	// Sanity check
	assert(queue.len(parser.tokens) > 0)

	indentation := exparr_last(&parser.stack).indentation
	itok := queue.get_ptr(&parser.tokens, 0)

	if itok.indentation <= indentation {
		return {from = itok.from}, true
	} else {
		return itok.token, true
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
	if found do advance_token(parser)
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

		advance_token(parser)
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
		advance_token(parser)
		exparr_push(&sp.segments, tok)
		sp.size += len(tok.content)
	case .Space, .Newline:
		advance_token(parser)

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

parse_string :: proc(parser: ^Parser) -> (res: string, ok: bool) {
	sp := string_parser_mk(parser)
	for do (string_parser_run(parser, &sp) or_return) or_break
	return string_parser_end(parser, &sp), true
}
// }}}
// {{{ Inline parsing
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
	tok := get_token(parser) or_return
	res: Inline_Markup
	switch tok.kind {
	case .Space, .Newline:
		advance_token(parser)
		res.kind = .Space
		if ip.elements.len == 0 || exparr_last(&ip.elements).kind == .Space {
			return true, true
		}
	case .Word:
		advance_token(parser)
		res.kind = .Text
		res.raw = tok.content
	case .None, .LCurly, .RCurly, .Eof:
		return false, true
	case .Apparition:
		for id in INLINE_APPARITIONS {
			name := APPARITIONS[id].name
			if tok.content == name {
				advance_token(parser)
				parsed := parse_apparition(parser, id, tok) or_return
				res = parsed.im
				break
			}
		}
	}

	exparr_push(&ip.elements, res)
	return true, true
}
// }}}
// {{{ Block parsing
Block_Parser :: struct {
	elements: Exparr(Block_Markup, 3),
}

block_parser_mk :: proc(parser: ^Parser) -> (res: Block_Parser) {
	res.elements.allocator = parser.allocator
	return res
}

block_parser_end :: proc(parser: ^Parser, bp: ^Block_Parser) -> (res: Block_Markup) {
	if bp.elements.len == 1 {
		res = exparr_last(&bp.elements)^
	} else {
		res = {
			kind = .Many,
			many = bp.elements,
		}
	}

	return res
}

block_parser_run :: proc(parser: ^Parser, bp: ^Block_Parser) -> (consumed: bool, ok: bool) {
	tok := get_token(parser) or_return

	if tok.kind == .Apparition {
		for id in BLOCK_APPARITIONS {
			name := APPARITIONS[id].name
			if tok.content == name {
				advance_token(parser)
				parsed := parse_apparition(parser, id, tok) or_return
				exparr_push(&bp.elements, parsed.bm)
				return true, true
			}
		}
	}

	// Paragraph/implicit inline handling
	if tok.kind == .Word || tok.kind == .Apparition {
		paragraph := inline_parser_mk(parser)

		last_was_nl := false
		progressed := false
		for {
			tok := get_token(parser) or_return
			if tok.kind == .Eof || tok.kind == .None {
				break
			} else if tok.kind == .Newline {
				if last_was_nl {
					advance_token(parser)
					break
				} else {
					last_was_nl = true
				}
			} else if tok.kind != .Space {
				last_was_nl = false
			}

			progressed_now := inline_parser_run(parser, &paragraph) or_return
			progressed ||= progressed_now
			if !progressed_now do break
		}

		inline_element := inline_parser_end(parser, &paragraph)
		element := Block_Markup {
			kind      = .Paragraph,
			paragraph = new_clone(inline_element, parser.allocator),
		}

		exparr_push(&bp.elements, element)

		return progressed, true
	} else if tok.kind == .Space || tok.kind == .Newline {
		advance_token(parser)
		return true, true
	} else {
		return false, true
	}
}

parse_blocks :: proc(parser: ^Parser) -> (res: Block_Markup, ok: bool) {
	ip := block_parser_mk(parser)
	for do (block_parser_run(parser, &ip) or_return) or_break
	return block_parser_end(parser, &ip), true
}
// }}}

// {{{ Apparition types
// A parsing result whose type is not tracked
Parsing_Result :: struct #raw_union {
	im:     Inline_Markup,
	bm:     Block_Markup,
	string: string,
	bool:   bool,
}

// The type of underlying environment some apparition's body takes place in
Apparition_Ambience :: enum {
	Void,
	Block,
	Inline,
	String,
}

Apparition_Id :: enum {
	// Reusable
	String_Id,

	// Inline
	Ellipsis,
	Date_Compact,
	Date,
	Datetime,
	Emph,
	Strong,
	Strikethrough,
	Mono,
	Quote,
	LaTeX,
	Icon,
	Fn,
	Link_Label,
	Link,

	// Block
	Toc,
	Embed_Description,
	H1,
	H2,
	H3,
	H4,
	H5,
	H6,
	Thematic_Break,
	Blockquote,
	Img,
	Img_Src,
	Aside,
	Aside_Character,
	Aside_Title,
	Aside_Collapse,
}

// Fat-struct containing everything an apparition is given when constructing
// its result from its children
Apparition_Making_Kit :: struct {
	parser:   ^Parser,
	head:     Token,
	children: [Apparition_Id]Parsing_Result,
	ambience: Parsing_Result,
	res:      Parsing_Result,
}

Apparition :: struct {
	name:     string,
	based_on: Apparition_Ambience,
	children: bit_set[Apparition_Id],
	make:     proc(kit: ^Apparition_Making_Kit),
}

// A partially-run parser whose type is not tracked
Ambient_Parser :: struct #raw_union {
	sp: String_Parser,
	ip: Inline_Parser,
	bp: Block_Parser,
}
// }}}
// {{{ Apparition tables
@(rodata)
APPARITIONS: [Apparition_Id]Apparition = {
	.String_Id         = ap_string_id,
	.Ellipsis          = ap_ellipsis,
	.Date_Compact      = ap_date_compact,
	.Datetime          = ap_datetime,
	.Date              = ap_date,
	.Emph              = ap_emph,
	.Strong            = ap_strong,
	.Strikethrough     = ap_strikethrough,
	.Mono              = ap_mono,
	.Quote             = ap_quote,
	.LaTeX             = ap_latex,
	.Icon              = ap_icon,
	.Fn                = ap_fn,
	.Link_Label        = ap_link_label,
	.Link              = ap_link,
	.H1                = ap_h1,
	.H2                = ap_h2,
	.H3                = ap_h3,
	.H4                = ap_h4,
	.H5                = ap_h5,
	.H6                = ap_h6,
	.Toc               = ap_toc,
	.Embed_Description = ap_embed_description,
	.Thematic_Break    = ap_thematic_break,
	.Blockquote        = ap_blockquote,
	.Img_Src           = ap_img_src,
	.Img               = ap_img,
	.Aside_Title       = ap_aside_title,
	.Aside_Collapse    = ap_aside_collapse,
	.Aside_Character   = ap_aside_character,
	.Aside             = ap_aside,
}

@(rodata)
INLINE_APPARITIONS: bit_set[Apparition_Id] = {
	.Ellipsis,
	.Date,
	.Datetime,
	.Emph,
	.Strong,
	.Strikethrough,
	.Mono,
	.Quote,
	.LaTeX,
	.Icon,
	.Fn,
	.Link,
}

@(rodata)
BLOCK_APPARITIONS: bit_set[Apparition_Id] = {
	.Toc,
	.Embed_Description,
	.H1,
	.H2,
	.H3,
	.H4,
	.H5,
	.H6,
	.Thematic_Break,
	.Blockquote,
	.Img,
	.Aside,
}
// }}}
// {{{ Reusable apparitions
ap_string_id :: Apparition {
	name     = "id",
	based_on = .String,
}
// }}}
// {{{ Inline apparitions
ap_emph :: Apparition {
	name = "_",
	based_on = .Inline,
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Emph
		kit.res.im.inner = new_clone(kit.ambience.im)
	},
}

ap_strong :: Apparition {
	name = "*",
	based_on = .Inline,
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Strong
		kit.res.im.inner = new_clone(kit.ambience.im)
	},
}

ap_strikethrough :: Apparition {
	name = "~",
	based_on = .Inline,
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Strikethrough
		kit.res.im.inner = new_clone(kit.ambience.im)
	},
}

ap_mono :: Apparition {
	name = "~",
	based_on = .Inline,
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Mono
		kit.res.im.inner = new_clone(kit.ambience.im)
	},
}

ap_quote :: Apparition {
	name = "\"",
	based_on = .Inline,
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Quote
		kit.res.im.inner = new_clone(kit.ambience.im)
	},
}

ap_latex :: Apparition {
	name = "$",
	based_on = .String,
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .LaTeX
		kit.res.im.raw = kit.ambience.string
	},
}

ap_icon :: Apparition {
	name = "icon",
	based_on = .String,
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Icon
		kit.res.im.raw = kit.ambience.string
	},
}

ap_fn :: Apparition {
	name = "fn",
	based_on = .String,
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Fn
		kit.res.im.raw = kit.ambience.string
	},
}

ap_link_label :: Apparition {
	name     = "label",
	based_on = .Inline,
}

ap_link :: Apparition {
	name = "link",
	based_on = .String,
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Link
		kit.res.im.link.id = kit.ambience.string
		kit.res.im.link.label = new_clone(kit.children[.Link_Label].im)
	},
}

ap_ellipsis :: Apparition {
	name = "...",
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Ellipsis
	},
}

ap_date_compact :: Apparition {
	name = "compact",
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bool = true
	},
}

ap_make_date :: proc(kit: ^Apparition_Making_Kit) {
	if kit.children[.Date_Compact].bool {
		kit.res.im.time.compact = true
	}

	as_string := kit.ambience.string
	datetime, datetime_consumed := time.iso8601_to_time_utc(as_string)
	if datetime_consumed > 0 {
		kit.res.im.time.time = datetime
	} else {
		// Try to tack an empty timestamp at the end
		as_date_string := fmt.aprintf("%vT00:00:00+00:00", as_string)
		date, date_consumed := time.iso8601_to_time_utc(as_date_string)

		if date_consumed > 0 {
			kit.res.im.time.time = date
		} else {
			msg := fmt.aprintf("Invalid date(time): '%v'", as_string)
			kit.parser.error = {kit.head, msg}
		}
	}
}

ap_date :: Apparition {
	name = "date",
	based_on = .String,
	children = {.Date_Compact},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Date
		ap_make_date(kit)
	},
}

ap_datetime :: Apparition {
	name = "datetime",
	based_on = .String,
	children = {.Date_Compact},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Datetime
		ap_make_date(kit)
	},
}
// }}}
// {{{ Block apparitions
ap_img_src: Apparition : {name = "src", based_on = .String}
ap_img: Apparition : {
	name = "img",
	based_on = .Inline,
	children = {.Img_Src},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Image
		kit.res.bm.image.alt = new_clone(kit.ambience.im)
		kit.res.bm.image.src = kit.children[.Img_Src].string
	},
}

ap_toc :: Apparition {
	name = "toc",
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Toc
	},
}

ap_embed_description :: Apparition {
	name = "embed-description",
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Embed_Description
	},
}

ap_thematic_break :: Apparition {
	name = "---",
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Thematic_Break
	},
}

ap_blockquote :: Apparition {
	name = ">",
	based_on = .Block,
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Blockquote
		kit.res.bm.blockquote = new_clone(kit.ambience.bm)
	},
}

ap_make_heading :: proc(kit: ^Apparition_Making_Kit) {
	heading := Heading {
		contents = kit.ambience.im,
		level    = len(kit.head.content),
		id       = kit.children[.String_Id].string,
	}

	kit.res.bm.kind = .Heading
	kit.res.bm.heading = new_clone(heading)
}

ap_h1 :: Apparition {
	name = "#",
	based_on = .Inline,
	children = {.String_Id},
	make = proc(kit: ^Apparition_Making_Kit) {ap_make_heading(kit)},
}

ap_h2 :: Apparition {
	name = "##",
	based_on = .Inline,
	children = {.String_Id},
	make = proc(kit: ^Apparition_Making_Kit) {ap_make_heading(kit)},
}

ap_h3 :: Apparition {
	name = "###",
	based_on = .Inline,
	children = {.String_Id},
	make = proc(kit: ^Apparition_Making_Kit) {ap_make_heading(kit)},
}

ap_h4 :: Apparition {
	name = "####",
	based_on = .Inline,
	children = {.String_Id},
	make = proc(kit: ^Apparition_Making_Kit) {ap_make_heading(kit)},
}

ap_h5 :: Apparition {
	name = "#####",
	based_on = .Inline,
	children = {.String_Id},
	make = proc(kit: ^Apparition_Making_Kit) {ap_make_heading(kit)},
}

ap_h6 :: Apparition {
	name = "######",
	based_on = .Inline,
	children = {.String_Id},
	make = proc(kit: ^Apparition_Making_Kit) {ap_make_heading(kit)},
}

ap_aside_collapse :: Apparition {
	name = "collapse",
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bool = true
	},
}

ap_aside_title :: Apparition {
	name     = "title",
	based_on = .Inline,
}

ap_aside_character :: Apparition {
	name     = "character",
	based_on = .String,
}

ap_aside :: Apparition {
	name = "aside",
	based_on = .Block,
	children = {.String_Id, .Aside_Collapse, .Aside_Title, .Aside_Character},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Aside
		kit.res.bm.aside.id = kit.children[.String_Id].string
		kit.res.bm.aside.character = kit.children[.Aside_Character].string
		kit.res.bm.aside.title = new_clone(kit.children[.Aside_Title].im)
		kit.res.bm.aside.content = new_clone(kit.ambience.bm)
		kit.res.bm.aside.collapse = kit.children[.Aside_Collapse].bool
	},
}
// }}}
// {{{ Parse a single apparition
parse_apparition :: proc(
	parser: ^Parser,
	id: Apparition_Id,
	head: Token,
) -> (
	res: Parsing_Result,
	ok: bool,
) {
	apparition := APPARITIONS[id]
	ambient: Ambient_Parser

	#partial switch apparition.based_on {
	case .Inline:
		ambient.ip = inline_parser_mk(parser)
	case .Block:
		ambient.bp = block_parser_mk(parser)
	case .String:
		ambient.sp = string_parser_mk(parser)
	case .Void:
	case:
		panic("Invalid apparition based_on field")
	}

	kit: Apparition_Making_Kit = {
		parser = parser,
		head   = head,
	}

	if apparition.children != {} || apparition.based_on != .Void {
		apparition_arg_begin(parser, head) or_return
		outer: for {
			tok := get_token(parser) or_return
			if tok.kind == .Apparition {
				for cid in apparition.children {
					name := APPARITIONS[cid].name
					if tok.content == name {
						if mem.check_zero(mem.ptr_to_bytes(&kit.children[cid])) {
							advance_token(parser)
							kit.children[cid] = parse_apparition(parser, cid, tok) or_return
							continue outer
						} else {
							parser.error = {
								tok,
								fmt.aprintf(
									"Duplicate apparition: '\\%v'",
									name,
									allocator = parser.allocator,
								),
							}

							return {}, false
						}
					}
				}
			}

			progress: bool
			#partial switch apparition.based_on {
			case .Inline:
				progress = inline_parser_run(parser, &ambient.ip) or_return
			case .Block:
				progress = block_parser_run(parser, &ambient.bp) or_return
			case .String:
				progress = string_parser_run(parser, &ambient.sp) or_return
			case .Void:
				if tok.kind == .Newline || tok.kind == .Space {
					advance_token(parser)
					progress = true
				}
			}

			if !progress do break
		}
		apparition_arg_end(parser) or_return
	}

	underlying: Parsing_Result
	#partial switch apparition.based_on {
	case .Inline:
		underlying.im = inline_parser_end(parser, &ambient.ip)
	case .Block:
		underlying.bm = block_parser_end(parser, &ambient.bp)
	case .String:
		underlying.string = string_parser_end(parser, &ambient.sp)
	}

	kit.ambience = underlying

	if apparition.make == nil {
		assert(apparition.children == {})
		return underlying, true
	} else {
		context.allocator = parser.allocator
		apparition.make(&kit)
		if parser.error != {} do return kit.res, false
		return kit.res, true
	}
}
// }}}
