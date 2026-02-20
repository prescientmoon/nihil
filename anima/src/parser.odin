package anima

import "base:runtime"
import "core:flags"
import "core:fmt"
import "core:log"
import "core:mem"
import "core:strings"
import "core:time"

// {{{ The parser type
Surrounding_Apparition :: struct {
	indentation: uint,
	power:       uint,
	node:        ^Apt_Node,
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

Parser :: struct {
	// The allocator used for all the "results" of the parser, i.e. the things
	// that must live on past its lifetime.
	allocator:          runtime.Allocator,
	// The allocator used for all the temporary stuff the parser allocates.
	// In particular, non-static error messages are thrown in here.
	//
	// Note that the string/inline/markup parsers are not particularly efficient
	// in this regard. That is, the memory they allocate will not get reused for
	// the rest of the duration of the parser's runtime. This does not matter in
	// practice though, and is not worth optimizing.
	internal_allocator: runtime.Allocator,
	lexer:              Lexer,
	tokens:             Exparr(Indented_Token, 3),
	stack:              Exparr(Surrounding_Apparition, 3),
	error:              Parsing_Error,
	page:               Page,
}

mk_parser :: proc(
	source: string,
	allocator := context.allocator,
	internal_allocator := context.allocator,
) -> (
	parser: Parser,
	ok: bool,
) {
	lexer, lexer_ok := mk_lexer(source, allocator)
	parser.lexer = lexer

	parser.internal_allocator = internal_allocator
	parser.tokens.allocator = internal_allocator
	parser.stack.allocator = internal_allocator

	parser.allocator = allocator
	parser.page.toc.allocator = parser.allocator
	parser.page.footnotes.allocator = parser.allocator
	parser.page.links.allocator = parser.allocator
	parser.page.feeds.allocator = parser.allocator
	parser.page.changelog.allocator = parser.allocator

	exparr_push(&parser.stack, Surrounding_Apparition{})
	return parser, lexer_ok
}

// To be called once parsing has ended. Ensures the entire file has been
// consumed, erroring out otherwise.
parser_end :: proc(parser: ^Parser) {
	if parser.error == {} {
		assert(parser.stack.len == 1)

		tok, ok := get_indented_token(parser)
		if !ok {
			return
		} else if tok.kind != .Eof {
			parser.error = {tok, "I'm confused by this token"}
		}
	}
}
// }}}
// {{{ Lexer helpers
advance_token :: proc(parser: ^Parser) {
	exparr_pop(&parser.tokens)
}

get_token :: proc(parser: ^Parser) -> (tok: Token, ok: bool) {
	panic("deprecated")
}

get_indented_token :: proc(parser: ^Parser) -> (tok: Token, ok: bool) {
	// When we run out of tokens, we run the lexer until we hit a non-whitespace
	// character. These tokens will not get thrown out, but will get adjusted to
	// match the indentation of the following token.
	if parser.tokens.len == 0 {
		for {
			tok := tokenize(&parser.lexer) or_return
			itok := Indented_Token {
				token       = tok,
				indentation = tok.from.col,
			}

			exparr_push(&parser.tokens, itok)
			if tok.kind != .Space && tok.kind != .Newline do break
		}

		last_indent := exparr_last(parser.tokens).indentation
		exparr_reverse(parser.tokens)

		// Indent spaces/newlines as much as the following token
		for i in 1 ..< parser.tokens.len {
			exparr_get(parser.tokens, i).indentation = last_indent
		}
	}

	// Sanity check
	assert(parser.tokens.len > 0)

	indentation := exparr_last(parser.stack).indentation
	itok := exparr_last(parser.tokens)

	if itok.indentation <= indentation {
		return {from = itok.from}, true
	} else {
		return itok.token, true
	}
}
// }}}
// {{{ Strings
String_Parser :: struct {
	size:     uint,
	segments: Exparr(Token, 3),
}

string_parser_mk :: proc(parser: ^Parser) -> String_Parser {
	return {size = 0, segments = {allocator = parser.internal_allocator}}
}

string_parser_end :: proc(parser: ^Parser, sp: ^String_Parser) -> string {
	// Remove spirious trailing spaces
	if sp.segments.len > 0 && exparr_last(sp.segments).kind == .Space {
		exparr_pop(&sp.segments)
	}

	// Allocate builder for the output string. Should never grow past the given
	// size!
	builder := strings.builder_make_len_cap(0, int(sp.size), parser.allocator)

	// Sanity check: attempting to re-allocate the buffer will cause a panic!
	builder.buf.allocator = runtime.panic_allocator()

	for i in 0 ..< sp.segments.len {
		tok := exparr_get(sp.segments, i)
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

		if sp.segments.len == 0 || exparr_last(sp.segments).kind == .Space {
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
	res.elements.allocator = parser.internal_allocator
	return res
}

inline_parser_end :: proc(parser: ^Parser, ip: ^Inline_Parser) -> (res: Inline_Markup) {
	// Remove spirious trailing spaces
	if ip.elements.len > 0 && exparr_last(ip.elements).kind == .Space {
		exparr_pop(&ip.elements)
	}

	if ip.elements.len == 1 {
		res = exparr_last(ip.elements)^
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
	#partial switch tok.kind {
	case .Space, .Newline:
		advance_token(parser)
		res.kind = .Space
		if ip.elements.len == 0 || exparr_last(ip.elements).kind == .Space {
			return true, true
		}
	case .Word:
		advance_token(parser)
		res.kind = .Text
		res.raw = tok.content
	case .None, .Eof:
		return false, true
	case .Apparition:
		found: bool
		for id in INLINE_APPARITIONS {
			name := APPARITIONS[id].name
			if tok.content == name {
				advance_token(parser)
				parsed := parse_apparition(parser, id, tok) or_return
				res = parsed.im
				found = true
				break
			}
		}

		if !found {
			return false, true
		}
	case:
		fmt.panicf("Invalid token encountered: %v", tok.kind)
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
	res.elements.allocator = parser.internal_allocator
	return res
}

block_parser_end :: proc(parser: ^Parser, bp: ^Block_Parser) -> (res: Block_Markup) {
	if bp.elements.len == 1 {
		res = exparr_last(bp.elements)^
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

				// We gracefully allow .None elements since certain parsers (i.e. the
				// metadata ones) do not produce any actual elements.
				if parsed.bm.kind != .None do exparr_push(&bp.elements, parsed.bm)

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
	im:         Inline_Markup,
	bm:         Block_Markup,
	string:     string,
	bool:       bool,
	many:       Exparr(Parsing_Result),
	table_cell: Table_Cell,
	table_row:  Table_Row,
	timestamp:  time.Time,
}

// The type of underlying environment some apparition's body takes place in
Apparition_Ambience :: enum {
	Void,
	Block,
	Inline,
	String,
	Timestamp, // Used by date/datetime
}

Apparition_Id :: enum {
	// Reusable
	String_Id,
	String_Bg,
	String_Source,
	Inline_Caption,
	Inline_Label,
	Bool_Hidden,

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
	Link,

	// Block
	Table_Of_Contents,
	Embed_Description,
	H1,
	H2,
	H3,
	H4,
	H5,
	H6,
	Thematic_Break,
	Blockquote,
	Image,
	Aside,
	Aside_Character,
	Aside_Title,
	Aside_Collapse,
	Figure,
	Table,
	Table_Head,
	Table_Row,
	Table_Cell,
	Inline_List, // inline list
	Block_List, // block list
	List_Inline_Item,
	List_Block_Item,
	List_Ordered,
	Linkdef,
	Fndef,
	Codeblock,
	Codeblock_Lang,
	Page_Index,
	Page_Filters_Children,
	Page_Filters_Descendants,

	// Meta
	Change,
	Change_At,
	// Page_Config,
	// Page_Config_Draft,
	// Page_Config_Compact,
	// Page_Config_Sitemap_Priority,
	// Page_Config_Sitemap_Changefreq,
}

// Fat-struct containing everything an apparition is given when constructing
// its result from its children
Apparition_Making_Kit :: struct {
	page:     ^Page,
	parser:   ^Parser,
	head:     Token,
	children: [Apparition_Id]Parsing_Result, // 0 or 1 times
	ambience: Parsing_Result,
	res:      Parsing_Result,
}

Apparition_Child_Multiplicity :: enum {
	Invalid, // 0
	Optional, // 0 or 1
	Required, // 1
	Repeatable, // 0 or more
}

// An apparition always comes bundled with an argument, unless the ambience is
// set to Void, and no (repeated) children are given (such an example is the
// \... ellipsis apparition)
Apparition :: struct {
	name:              string,
	// Determines what parser is going to handle the tokens not corresponding to
	// any of the children
	based_on:          Apparition_Ambience,
	other_children:    [Apparition_Id]Apparition_Child_Multiplicity,
	// Apparitions that are allowed to appear at most once in the body. If their
	// presence is required, check this in the "make" procedure below
	children:          bit_set[Apparition_Id],
	// Like "children", except they can appear any number of times
	repeated_children: bit_set[Apparition_Id],
	// A procedure that collects the parsing results from the children/ambiance
	// and bundles it into the final structure
	make:              proc(kit: ^Apparition_Making_Kit),
}

// A partially-run parser whose type is not tracked by the compiler
Ambient_Parser :: struct #raw_union {
	sp: String_Parser,
	ip: Inline_Parser,
	bp: Block_Parser,
}
// }}}
// {{{ Apparition tables
@(rodata)
APPARITIONS: [Apparition_Id]Apparition = {
	.String_Id                = ap_string_id,
	.String_Bg                = ap_string_bg,
	.String_Source            = ap_string_source,
	.Inline_Caption           = ap_inline_caption,
	.Inline_Label             = ap_inline_label,
	.Bool_Hidden              = ap_bool_hidden,
	.Ellipsis                 = ap_ellipsis,
	.Date_Compact             = ap_date_compact,
	.Datetime                 = ap_datetime,
	.Date                     = ap_date,
	.Emph                     = ap_emph,
	.Strong                   = ap_strong,
	.Strikethrough            = ap_strikethrough,
	.Mono                     = ap_mono,
	.Quote                    = ap_quote,
	.LaTeX                    = ap_latex,
	.Icon                     = ap_icon,
	.Fn                       = ap_fn,
	.Link                     = ap_link,
	.H1                       = ap_h1,
	.H2                       = ap_h2,
	.H3                       = ap_h3,
	.H4                       = ap_h4,
	.H5                       = ap_h5,
	.H6                       = ap_h6,
	.Table_Of_Contents        = ap_table_of_contents,
	.Embed_Description        = ap_embed_description,
	.Thematic_Break           = ap_thematic_break,
	.Blockquote               = ap_blockquote,
	.Image                    = ap_image,
	.Aside_Title              = ap_aside_title,
	.Aside_Collapse           = ap_aside_collapse,
	.Aside_Character          = ap_aside_character,
	.Aside                    = ap_aside,
	.Figure                   = ap_figure,
	.Table                    = ap_table,
	.Table_Head               = ap_table_head,
	.Table_Row                = ap_table_row,
	.Table_Cell               = ap_table_cell,
	.Inline_List              = ap_inline_list,
	.Block_List               = ap_block_list,
	.List_Inline_Item         = ap_list_inline_item,
	.List_Block_Item          = ap_list_block_item,
	.List_Ordered             = ap_list_ordered,
	.Linkdef                  = ap_linkdef,
	.Fndef                    = ap_fndef,
	.Codeblock                = ap_codeblock,
	.Codeblock_Lang           = ap_codeblock_lang,
	.Page_Index               = ap_page_index,
	.Page_Filters_Children    = ap_page_filter_children,
	.Page_Filters_Descendants = ap_page_filter_descendants,
	.Change                   = ap_change,
	.Change_At                = ap_change_at,
	// .Page_Config                    = ap_page_config,
	// .Page_Config_Draft              = ap_page_config_draft,
	// .Page_Config_Compact            = ap_page_config_compact,
	// .Page_Config_Sitemap_Priority   = ap_page_config_sitemap_priority,
	// .Page_Config_Sitemap_Changefreq = ap_page_config_sitemap_changefreq,
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
	.Table_Of_Contents,
	.Embed_Description,
	.H1,
	.H2,
	.H3,
	.H4,
	.H5,
	.H6,
	.Thematic_Break,
	.Blockquote,
	.Image,
	.Aside,
	.Figure,
	.Table,
	.Inline_List,
	.Block_List,
	.Linkdef,
	.Fndef,
	.Codeblock,
	.Page_Index,
	.Change,
	// .Page_Config,
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
	assert((apparition.children & apparition.repeated_children) == {})

	#partial switch apparition.based_on {
	case .Inline:
		ambient.ip = inline_parser_mk(parser)
	case .Block:
		ambient.bp = block_parser_mk(parser)
	case .String, .Timestamp:
		ambient.sp = string_parser_mk(parser)
	case .Void:
	case:
		panic("Invalid apparition based_on field")
	}

	kit: Apparition_Making_Kit = {
		parser = parser,
		head   = head,
	}

	if apparition.children != {} ||
	   apparition.repeated_children != {} ||
	   apparition.based_on != .Void {
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
									allocator = parser.internal_allocator,
								),
							}

							return {}, false
						}
					}
				}

				for cid in apparition.repeated_children {
					name := APPARITIONS[cid].name
					if tok.content == name {
						kit.children[cid].many.allocator = parser.internal_allocator
						advance_token(parser)
						exparr_push(
							&kit.children[cid].many,
							parse_apparition(parser, cid, tok) or_return,
						)
						continue outer
					}
				}
			}

			progress: bool
			#partial switch apparition.based_on {
			case .Inline:
				progress = inline_parser_run(parser, &ambient.ip) or_return
			case .Block:
				progress = block_parser_run(parser, &ambient.bp) or_return
			case .String, .Timestamp:
				progress = string_parser_run(parser, &ambient.sp) or_return
			case .Void:
				if tok.kind == .Newline || tok.kind == .Space {
					advance_token(parser)
					progress = true
				}

				if tok.kind == .Word {
					parser.error = {tok, "Words are not allowed in this context"}
					return {}, false
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
	case .Timestamp:
		as_string := string_parser_end(parser, &ambient.sp)
		datetime, datetime_consumed := time.iso8601_to_time_utc(as_string)

		if datetime_consumed > 0 {
			underlying.timestamp = datetime
		} else {
			// Try to tack an empty timestamp at the end
			as_date_string := fmt.aprintf(
				"%vT00:00:00+00:00",
				as_string,
				allocator = parser.internal_allocator,
			)
			date, date_consumed := time.iso8601_to_time_utc(as_date_string)

			if date_consumed > 0 {
				underlying.timestamp = date
			} else {
				msg := fmt.aprintf(
					"Invalid date(time): '%v'",
					as_string,
					allocator = parser.internal_allocator,
				)
				kit.parser.error = {kit.head, msg}
				return underlying, false
			}
		}
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
// {{{ Apparition arguments
apparition_arg_begin :: proc(parser: ^Parser, apparition: Token) -> (ok: bool) {
	panic("deprecated")
}

apparition_arg_end :: proc(parser: ^Parser) -> (ok: bool) {
	panic("deprecated")
}
// }}}

// {{{ Reusable apparitions
ap_string_id: Apparition : {name = "id", based_on = .String}
ap_string_bg: Apparition : {name = "bg", based_on = .String}
ap_string_source: Apparition : {name = "src", based_on = .String}

ap_inline_caption: Apparition : {name = "caption", based_on = .Inline}
ap_inline_label: Apparition : {name = "label", based_on = .Inline}

ap_bool_hidden :: Apparition {
	name = "hidden",
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bool = true
	},
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

ap_link :: Apparition {
	name = "link",
	based_on = .String,
	children = {.Inline_Label},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.im.kind = .Link
		kit.res.im.link.id = kit.ambience.string
		kit.res.im.link.label = new_clone(kit.children[.Inline_Label].im)
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

	kit.res.im.time.time = kit.ambience.timestamp
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
ap_image: Apparition : {
	name = "img",
	based_on = .Inline,
	children = {.String_Source},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Image
		kit.res.bm.image.alt = new_clone(kit.ambience.im)
		kit.res.bm.image.source = kit.children[.String_Source].string
		if kit.res.bm.image.source == {} {
			kit.parser.error = {kit.head, "Image has no source"}
		}
	},
}

ap_table_of_contents :: Apparition {
	name = "toc",
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Table_Of_Contents
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

ap_figure :: Apparition {
	name = "figure",
	based_on = .Block,
	children = {.Inline_Caption},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Figure
		kit.res.bm.figure.caption = new_clone(kit.children[.Inline_Caption].im)
		kit.res.bm.figure.content = new_clone(kit.ambience.bm)
		if kit.res.bm.figure.caption.kind == .None {
			kit.parser.error = {kit.head, "Figure has no caption"}
		}
	},
}

ap_make_table_row :: proc(kit: ^Apparition_Making_Kit) {
	kit.res.table_row.cells.allocator = kit.parser.allocator
	cells := kit.children[.Table_Cell].many
	for i in 0 ..< cells.len {
		cell := exparr_get(cells, i)
		exparr_push(&kit.res.table_row.cells, cell.table_cell)
	}
}

ap_table_cell :: Apparition {
	name = "cell",
	based_on = .Inline,
	children = {.String_Bg},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.table_cell.bg = kit.children[.String_Bg].string
		kit.res.table_cell.content = new_clone(kit.ambience.im)
	},
}

ap_table_head :: Apparition {
	name = "head",
	based_on = .Void,
	repeated_children = {.Table_Cell},
	make = proc(kit: ^Apparition_Making_Kit) {ap_make_table_row(kit)},
}

ap_table_row :: Apparition {
	name = "row",
	based_on = .Void,
	repeated_children = {.Table_Cell},
	make = proc(kit: ^Apparition_Making_Kit) {ap_make_table_row(kit)},
}

ap_table :: Apparition {
	name = "table",
	based_on = .Void,
	children = {.Table_Head, .Inline_Caption},
	repeated_children = {.Table_Row},
	other_children = #partial{
		.Inline_Caption = .Optional,
		.Table_Head = .Required,
		.Table_Row = .Repeatable,
	},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Table
		kit.res.bm.table.caption = new_clone(kit.children[.Inline_Caption].im)
		kit.res.bm.table.head = new_clone(kit.children[.Table_Head].table_row)
		kit.res.bm.table.rows.allocator = kit.parser.allocator

		rows := kit.children[.Table_Row].many
		for i in 0 ..< rows.len {
			row := exparr_get(rows, i)
			exparr_push(&kit.res.bm.table.rows, row.table_row)
		}
	},
}

ap_list_inline_item :: Apparition {
	name     = "item",
	based_on = .Inline,
}

ap_list_block_item :: Apparition {
	name     = "item",
	based_on = .Block,
}

ap_list_ordered :: Apparition {
	name = "ordered",
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bool = true
	},
}

ap_block_list :: Apparition {
	name = "blist",
	children = {.List_Ordered},
	repeated_children = {.List_Block_Item},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .BList
		kit.res.bm.blist.ordered = kit.children[.List_Ordered].bool
		kit.res.bm.blist.items.allocator = kit.parser.allocator

		items := kit.children[.List_Block_Item].many
		for i in 0 ..< items.len {
			item := exparr_get(items, i)
			exparr_push(&kit.res.bm.blist.items, item.bm)
		}
	},
}

ap_inline_list :: Apparition {
	name = "ilist",
	children = {.List_Ordered},
	repeated_children = {.List_Inline_Item},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .IList
		kit.res.bm.ilist.ordered = kit.children[.List_Ordered].bool
		kit.res.bm.ilist.items.allocator = kit.parser.allocator

		items := kit.children[.List_Inline_Item].many
		for i in 0 ..< items.len {
			item := exparr_get(items, i)
			exparr_push(&kit.res.bm.ilist.items, item.im)
		}
	},
}

ap_linkdef :: Apparition {
	name = "linkdef",
	based_on = .String,
	children = {.String_Id, .Inline_Label},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Linkdef

		linkdef := Linkdef {
			target = kit.ambience.string,
			label  = kit.children[.Inline_Label].im,
			id     = kit.children[.String_Id].string,
		}

		kit.res.bm.linkdef = new_clone(linkdef)
		if linkdef.id == {} {
			kit.parser.error = {kit.head, "Linkdef has no ID"}
		} else if linkdef.target == {} {
			kit.parser.error = {kit.head, "Linkdef has no target"}
		}
	},
}

ap_fndef :: Apparition {
	name = "fndef",
	based_on = .Block,
	children = {.String_Id},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Fndef

		fndef := Fndef {
			id       = kit.children[.String_Id].string,
			contents = kit.ambience.bm,
		}

		kit.res.bm.fndef = new_clone(fndef)

		if fndef.id == {} {
			kit.parser.error = {kit.head, "Fndef has no ID"}
		}
	},
}

ap_codeblock_lang :: Apparition {
	name     = "lang",
	based_on = .String,
}

ap_codeblock :: Apparition {
	name = "code",
	children = {.String_Source, .Codeblock_Lang},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Codeblock
		kit.res.bm.codeblock.source = kit.children[.String_Source].string
		kit.res.bm.codeblock.lang = kit.children[.Codeblock_Lang].string

		if kit.res.bm.codeblock.source == {} {
			kit.parser.error = {kit.head, "Codeblock has no source"}
		} else if kit.res.bm.codeblock.lang == {} {
			kit.parser.error = {kit.head, "Codeblock has no language"}
		}
	},
}

ap_page_filter_children :: Apparition {
	name     = "children",
	based_on = .String,
}

ap_page_filter_descendants :: Apparition {
	name     = "descendants",
	based_on = .String,
}

ap_page_index :: Apparition {
	name = "page-index",
	children = {.Page_Filters_Descendants, .Page_Filters_Children, .Bool_Hidden},
	make = proc(kit: ^Apparition_Making_Kit) {
		kit.res.bm.kind = .Page_Index
		filters := &kit.res.bm.page_index.filters
		filters.children = auto_cast kit.children[.Page_Filters_Children].string
		filters.descendants = auto_cast kit.children[.Page_Filters_Descendants].string
		filters.hidden = kit.children[.Bool_Hidden].bool

		if filters.descendants != {} && filters.children != {} {
			kit.parser.error = {kit.head, "Conflicting page filters given"}
		}
	},
}
// }}}
// {{{ Meta apparitions
ap_change_at :: Apparition {
	name     = "at",
	based_on = .Timestamp,
}

ap_change :: Apparition {
	name = "change",
	based_on = .Block,
	children = {.Change_At},
	make = proc(kit: ^Apparition_Making_Kit) {
		change := Change {
			at   = kit.children[.Change_At].timestamp,
			body = kit.ambience.bm,
		}

		exparr_push(&kit.parser.page.changelog, change)

		if change.body.kind == .None {
			kit.parser.error = {kit.head, "Change has no description"}
		} else if change.at == {} {
			kit.parser.error = {kit.head, "Change has no timestamp"}
		}
	},
}
// }}}

// {{{ Apparition tree parsing
Apt_Parsing_Error :: enum {
	None,
	Lexer_Error,
}

parse_apt :: proc(parser: ^Parser, builder: ^Apt_Builder) -> (err: Apt_Parsing_Error) {
	for {
		tok, ok := get_indented_token(parser)
		if !ok do return .Lexer_Error
		switch tok.kind {
		case .Colon, .Bang:
			advance_token(parser)
			node := tb_leaf(builder, Apt_Node{kind = .Leaf, tok = tok})
			node.tok.kind = .Word
		case .LCurly:
			advance_token(parser)
			node := tb_leaf(builder, Apt_Node{kind = .Leaf, tok = tok})
			node.tok.kind = .Word
			exparr_push(&node.errors, Parsing_Error{tok, "Unexpected block opener"})
		case .RCurly:
			power := cast(uint)len(tok.content)

			for i := parser.stack.len - 1; cast(int)i >= 0; i -= 1 {
				elem := exparr_get(parser.stack, i)
				if elem.power == power && elem.kind == .Bracketed {
					return .None // End of the current scope!
				} else if elem.power > power {
					break
				}
			}

			advance_token(parser)
			node := tb_leaf(builder, Apt_Node{kind = .Leaf, tok = tok})
			node.tok.kind = .Word
			elem := exparr_last(parser.stack)
			exparr_push(&elem.node.errors, Parsing_Error{tok, "Invalid block closer"})
		case .None, .Eof:
			return .None // End of the current scope!
		case .Word, .Space, .Newline:
			advance_token(parser)
			tb_leaf(builder, Apt_Node{kind = .Leaf, tok = tok})
		case .Apparition:
			advance_token(parser)
			node := tb_node_begin(builder, Apt_Node{kind = .Node, tok = tok})
			node.errors.allocator = parser.allocator
			defer tb_node_end(builder)

			elem: Surrounding_Apparition = {
				node = node,
			}

			next_tok, ok := get_indented_token(parser)
			if !ok do return .Lexer_Error
			#partial switch next_tok.kind {
			case .Colon:
				advance_token(parser)
				elem.power = len(next_tok.content)
			case .LCurly:
				advance_token(parser)
				elem.kind = .Bracketed
				elem.power = len(next_tok.content)
			case .Bang:
				advance_token(parser)
				elem.kind = .Ambient
				elem.power = len(next_tok.content)
			}

			switch elem.kind {
			case .Ambient, .Bracketed:
				elem.indentation = exparr_last(parser.stack).indentation
			case .Indented:
				elem.indentation = tok.from.col
			}

			exparr_push(&parser.stack, elem)
			defer exparr_pop(&parser.stack)

			first_tok, last_tok: Token
			first_tok, ok = get_indented_token(parser)
			if !ok do return .Lexer_Error
			first_pos := next_tok.from.index

			err := parse_apt(parser, builder)
			switch err {
			case .None:
			case .Lexer_Error:
				return .Lexer_Error
			}

			last_tok, ok = get_indented_token(parser)
			if !ok do return .Lexer_Error

			last_tok_power := cast(uint)len(last_tok.content)
			switch elem.kind {
			case .Indented, .Ambient:
			case .Bracketed:
				if last_tok.kind == .RCurly && last_tok_power == elem.power {
					advance_token(parser)

					last_tok, ok = get_indented_token(parser)
					if !ok do return .Lexer_Error
				} else {
					msg := fmt.aprintf(
						"Expected a closer for '%v' at %v",
						next_tok.content,
						next_tok.from,
						allocator = parser.allocator,
					)

					exparr_push(&node.errors, Parsing_Error{tok, msg})
				}
			}

			last_pos := last_tok.from.index + cast(uint)len(last_tok.content)
			node.raw = parser.lexer.source[first_pos:last_pos]
		}
	}

	return .None
}
// }}}

// Evaluation
// {{{ Types
Scope :: struct {
	parent:  ^Scope,
	// NOTE(storage): We could use a tree structure for scopes, thus not requiring
	// exponential arrays of members everywhere.
	members: Exparr(Binding),
}

Binding :: struct {
	name: string,
	spec: Apparition_Spec,
}

Evaluator :: struct {
	internal_allocator: runtime.Allocator,
	allocator:          runtime.Allocator,
	scope:              ^Scope,
	caller:             ^Scope, // Used for delayed bindings
	// NOTE(storage): This is always the same across all modified evaluators. We
	// could therefore move this (together with the allocators above) to a single
	// "evaluator context" struct, and simply reference that everywhere instead.
	errors:             ^Exparr(Parsing_Error),
}

// Note that:
// - An associative argument cannot be quoted (there's no children to quote!)
// - An ambient argument cannot have any other flags (due to its parsing being
//   completely different)
Apparition_Argument_Flag :: enum {
	Unique, // Cannot appear more than once.
	Required, // Cannot appear less than once.
	Associative, // Contains no children. Note that .Associative implies .Unique.
	Quoted, // Arguments of this kind have their evaluation delayed until usage.
	Ambient, // Collects every child that doesn't fit anywhere else.
}

Apparition_Argument :: struct {
	name:  string,
	flags: bit_set[Apparition_Argument_Flag],
}

Dynamic_Apparition_Flag :: enum {
	// Hoisted apparitions get processed before everything else in the scope
	Hoisted,
}

// Used for apparitions that are hardcoded in the evaluator itself. Such
// apparitions are even more fundamental than etternal apparitions (see
// etternal.anima), since defining etternal apparitions requires a base
// level of syntax (in particular, access to \skeleton and \arg).
Primordial_Apparition_Spec :: enum {
	Skeleta,
	Skeleton,
	Arg,
}

// Used for referencing apparitions defined in the etternal.anima file. Like
// primordial apparitions, such specs look dynamic from the outside, but
// produce no proper output based on anima-code, and are instead parsed by
// odin code into odin-specific structures.
//
// Another way to think of etternal apparitions is as "type definitions" for
// dynamic apparitions. Indeed, every dynamic apparition carries an etternal
// "skeleton" (see the dynamic apparition spec struct).
Etternal_Apparition_Spec :: struct {
	flags:     bit_set[Dynamic_Apparition_Flag],
	// What apparitions can appear as the children?
	// NOTE(storage): we could instead use an intrusive linked list backed by
	// a flat buffer or by an arena here, although I'm not sure that would
	// simplify usage in any way.
	arguments: Exparr(Apparition_Argument),
}

// User defined apparition
Dynamic_Apparition_Spec :: struct {
	using skeleton: Etternal_Apparition_Spec,
	// Where is this defined?
	scope:          ^Scope,
	// The return value of the apparition at hand.
	body:           Exp_Apf,
}

// One uses apparition syntax in order to pass arguments to a dynamic
// apparition. The "fake" apparitions that do nothing but append to the
// argument list are known as "rigid" apparitions.
Rigid_Apparition_Spec :: struct {
	flags: bit_set[Apparition_Argument_Flag],
}

// The concrete value of an argument passed to the current scope.
Bound_Apparition_Spec :: struct {
	instances: Evaluated_Argument_Instances,
}

Apparition_Spec :: union #no_nil {
	Dynamic_Apparition_Spec,
	Rigid_Apparition_Spec,
	Bound_Apparition_Spec,
	Primordial_Apparition_Spec,
	Etternal_Apparition_Spec,
}

// NOTE(storage): Using slices of exponential arrays for arguments feels weird.
// An alternative approach would be throwing the trees into a big buffer
// somewhere, assembling them into a "sea of trees" structure.
Evaluated_Argument_Instances :: Exparr(Evaluated_Argument)
Evaluated_Argument :: struct {
	at:    Token,
	value: Exp_Apf,
}

// The arguments appear in the same order as dictated by the spec. Every
// argument can appear one or more times. The count is guaranteed to respect
// the argument's flags (i.e. unique arguments cannot appear more than once).
Apparition_Evaluation :: distinct []Evaluated_Argument_Instances
// }}}
// {{{ Apparition spec properties
ap_spec_is_associative :: proc(spec: Apparition_Spec) -> bool {
	switch inner in spec {
	case Rigid_Apparition_Spec:
		return .Associative in inner.flags
	case Dynamic_Apparition_Spec:
		return inner.arguments.len == 0
	case Bound_Apparition_Spec:
		return true
	case Primordial_Apparition_Spec:
		return false
	case Etternal_Apparition_Spec:
		panic("Unimplemented")
	}

	panic("impossible")
}
// }}}
// {{{ Apparition forest iterator
// Represents a collection of apparition trees, or an apparition forest for
// short. The forest can internally be represented as one of the following:
// - A single tree
// - A linked forest (this is what the parser returns)
// - A distributed forest (i.e. an ad-hoc collection of trees)
//
// In the future, I might attempt to simplify this by reducing the usage of
// non-distributed forests (or even renaming the two kinds), but for now this
// will have to do.
Varied_Apf :: union #no_nil {
	Apt,
	Linked_Apf,
	Exp_Apf,
}

Varied_Apf_Iter :: struct {
	vapf:   Varied_Apf,
	index:  uint, // The amount of trees extracted so far
	offset: uint, // The minimum index after dropping whitespace
	len:    uint, // The number of elements before the trailing whitespace

	// Stores the next apt in the chain in the case of linked forests).
	// Remains zeroed-out in the case of empty linked forests or exponential
	// forests.
	apt:    Apt,
}

vapf_iter_mk :: proc(
	vapf: Varied_Apf,
	drop_left_ws := true,
	drop_right_ws := true,
) -> (
	out: Varied_Apf_Iter,
) {
	WHITESPACE: bit_set[Token_Kind] : {.Space, .Newline}
	out.vapf = vapf

	// Calculate lengths
	switch inner in vapf {
	case Apt:
		out.len = 1
	case Linked_Apf:
		node := inner.next
		for node != nil {
			defer node = node.siblings.next
			out.len += 1
		}

		if inner.next != nil do out.apt = inner.next^
	case Exp_Apf:
		out.len = inner.len
	}

	if drop_left_ws {
		leading_ws: uint = 0
		switch inner in vapf {
		case Apt:
		case Linked_Apf:
			node := inner.next
			for node != nil {
				defer node = node.siblings.next
				if node.data.tok.kind in WHITESPACE {
					leading_ws += 1
				} else {
					out.apt = node^
					break
				}
			}
		case Exp_Apf:
			for i in 0 ..< inner.len {
				node := exparr_get(inner, i)
				if node.data.tok.kind not_in WHITESPACE {
					leading_ws = i
					break
				}
			}
		}

		out.offset = leading_ws
		log.assert(out.len >= leading_ws)
		out.len -= leading_ws
	}

	if drop_right_ws {
		trailing_ws: uint = 0
		switch inner in vapf {
		case Apt:
		case Linked_Apf:
			node := inner.prev
			for node != nil {
				defer node = node.siblings.prev
				if node.data.tok.kind in WHITESPACE {
					trailing_ws += 1
				} else {
					break
				}
			}
		case Exp_Apf:
			for i := inner.len - 1; int(i) >= 0; i -= 1 {
				node := exparr_get(inner, i)
				if node.data.tok.kind not_in WHITESPACE {
					trailing_ws = inner.len - i - 1
					break
				}
			}
		}

		// The second condition is there to ensure the trailing and leading
		// whitepsace spans are not referring to the same section
		if drop_right_ws && out.len != 0 {
			log.assert(out.len >= trailing_ws)
			out.len -= trailing_ws
		}
	}

	return out
}

vapf_iter_next :: proc(iter: ^Varied_Apf_Iter) -> (out: Apt, ok: bool) {
	(iter.index < iter.len) or_return
	defer if ok do iter.index += 1

	switch inner in iter.vapf {
	case Apt:
		out = inner
	case Linked_Apf:
		out = iter.apt
		if out.siblings.next == nil {
			log.assertf(
				iter.index == iter.len - 1,
				"Expected index %v to be %v",
				iter.index,
				iter.len - 1,
			)
		} else do iter.apt = out.siblings.next^
	case Exp_Apf:
		out = exparr_get(inner, iter.offset + iter.index)^
	}

	return out, true
}
// }}}
// {{{ Apparition spec arguments
Apparition_Spec_Arguments_Iterator :: struct {
	index: uint,
	spec:  Apparition_Spec,
}

ap_spec_arg_count :: proc(spec: Apparition_Spec) -> uint {
	switch inner in spec {
	case Dynamic_Apparition_Spec:
		return inner.arguments.len
	case Rigid_Apparition_Spec:
		return 1
	case Bound_Apparition_Spec:
		return 0
	case Primordial_Apparition_Spec:
		switch inner {
		case .Skeleta:
			return 1
		case .Skeleton:
			return 3
		case .Arg:
			return 5
		}
	case Etternal_Apparition_Spec:
		panic("unimplemented")
	}

	panic("impossible")
}

ap_spec_arg_iter_mk :: proc(spec: Apparition_Spec) -> Apparition_Spec_Arguments_Iterator {
	return {spec = spec}
}

ap_spec_arg_iter_next :: proc(
	iter: ^Apparition_Spec_Arguments_Iterator,
) -> (
	arg: Apparition_Argument,
	index: uint,
	ok: bool,
) {
	(iter.index < ap_spec_arg_count(iter.spec)) or_return
	defer if ok do iter.index += 1

	switch inner in iter.spec {
	case Dynamic_Apparition_Spec:
		arg = exparr_get(inner.arguments, iter.index)^
	case Rigid_Apparition_Spec:
		arg.name = "__ambient"
		arg.flags = {.Ambient}
	case Bound_Apparition_Spec:
		panic("impossible")
	case Primordial_Apparition_Spec:
		switch inner {
		case .Skeleta:
			@(rodata, static)
			args: []Apparition_Argument = {{"skeleton", {}}}
			arg = args[iter.index]
		case .Skeleton:
			@(rodata, static)
			args: []Apparition_Argument = {
				{"name", {.Ambient}},
				{"arg", {}},
				{"hoisted", {.Associative}},
				{"return", {.Unique, .Quoted}},
			}

			arg = args[iter.index]
		case .Arg:
			@(rodata, static)
			args: []Apparition_Argument = {
				{"name", {.Ambient}},
				{"ambient", {.Associative}},
				{"unique", {.Associative}},
				{"required", {.Associative}},
				{"assoc", {.Associative}},
			}

			arg = args[iter.index]
		}
	case Etternal_Apparition_Spec:
		panic("unimplemented")
	}

	index = iter.index
	return arg, index, true
}

ap_spec_find_ambient_arg :: proc(spec: Apparition_Spec) -> (index: uint, ok: bool) {
	iter := ap_spec_arg_iter_mk(spec)

	for arg, i in ap_spec_arg_iter_next(&iter) {
		if .Ambient in arg.flags {
			return i, true
		}
	}

	return
}

ap_spec_find_arg :: proc(spec: Apparition_Spec, name: string) -> (index: uint, ok: bool) {
	iter := ap_spec_arg_iter_mk(spec)

	for arg, i in ap_spec_arg_iter_next(&iter) {
		if name == arg.name {
			return i, true
		}
	}

	return
}

ap_spec_get_arg :: proc(
	spec: Apparition_Spec,
	args: Apparition_Evaluation,
	arg_name: string,
) -> Evaluated_Argument_Instances {
	arg_ix, has_arg := ap_spec_find_arg(spec, arg_name)
	log.assertf(has_arg, "Cannot find argument %v", arg_name)
	return args[arg_ix]
}
// }}}
// {{{ Scope operations
scope_and_argument_lookup :: proc(
	spec: Apparition_Spec,
	scope: Scope,
	name: string,
) -> (
	binding: Binding,
	ok: bool,
) {
	scope_lookup :: proc(scope: Scope, name: string) -> (binding: Binding, ok: bool) {
		for i := scope.members.len - 1; int(i) >= 0; i -= 1 {
			binding := exparr_get(scope.members, i)
			if binding.name == name {
				return binding^, true
			}
		}

		if scope.parent != nil {
			return scope_lookup(scope.parent^, name)
		}

		return {}, false
	}

	iter := ap_spec_arg_iter_mk(spec)
	for arg in ap_spec_arg_iter_next(&iter) {
		if arg.name == name {
			binding.name = name
			binding.spec = Rigid_Apparition_Spec {
				flags = arg.flags,
			}

			return binding, true
		}
	}

	return scope_lookup(scope, name)
}

nest_scope :: proc(eval: Evaluator, scope: ^Scope) -> ^Scope {
	scope := scope
	nested_scope := new(Scope, eval.allocator)
	nested_scope.parent = scope
	nested_scope.members.allocator = scope.members.allocator
	return nested_scope
}
// }}}
// {{{ Whole apparition parsing
mk_apparition_parsing_args :: proc(
	eval: Evaluator,
	spec: Apparition_Spec,
	at: Token,
) -> (
	args: Apparition_Evaluation,
) {
	args = make_slice(Apparition_Evaluation, ap_spec_arg_count(spec), eval.allocator)
	iter := ap_spec_arg_iter_mk(spec)
	for arg_spec, i in ap_spec_arg_iter_next(&iter) {
		instances := &args[i]
		instances.allocator = eval.allocator
		if .Ambient in arg_spec.flags {
			arg: Evaluated_Argument
			arg.at = at
			arg.value.allocator = eval.allocator
			exparr_push(instances, arg)
		}
	}

	return args
}

apt_parse_apparition_application :: proc(
	eval: Evaluator,
	spec: Apparition_Spec,
	at: Token,
	body: Varied_Apf,
) -> (
	nested_arguments: Apparition_Evaluation,
	ok: bool,
) {
	nested_arguments = mk_apparition_parsing_args(eval, spec, at)

	nested_eval := eval
	nested_eval.scope = nest_scope(eval, eval.scope)
	apt_parse(nested_eval, spec, body, nested_arguments)
	// TODO: enforce argument flags
	return nested_arguments, true
}
// }}}
// {{{ Apparition definitions parsing
apt_parse_arg :: proc(
	eval: Evaluator,
	spec: Apparition_Spec,
	partial_spec: ^Etternal_Apparition_Spec,
	raw_arg: Evaluated_Argument,
) -> (
	ok: bool,
) {
	evaluated_arg := apt_parse_apparition_application(
		eval,
		spec,
		raw_arg.at,
		raw_arg.value,
	) or_return

	arg: Apparition_Argument
	arg.name = arg_get_name(eval, spec, raw_arg.at, evaluated_arg, "name") or_return

	if is_arg_given(spec, evaluated_arg, "ambient") {
		arg.flags += {.Ambient}
	}

	if is_arg_given(spec, evaluated_arg, "unique") {
		arg.flags += {.Unique}
	}

	if is_arg_given(spec, evaluated_arg, "required") {
		arg.flags += {.Required}
	}

	if is_arg_given(spec, evaluated_arg, "assoc") {
		arg.flags += {.Associative}
	}

	if .Ambient in arg.flags && (arg.flags - {.Ambient}) != {} {
		arg.flags = {.Ambient}
		exparr_push(
			eval.errors,
			Parsing_Error{raw_arg.at, "Ambient arguments cannot have other flags"},
		)
	} else if .Associative in arg.flags && .Quoted in arg.flags {
		arg.flags = {.Associative}
		exparr_push(
			eval.errors,
			Parsing_Error {
				raw_arg.at,
				"Argument cannot be quoted and associative at the same time",
			},
		)
	}

	for i in 0 ..< partial_spec.arguments.len {
		other_arg := exparr_get(partial_spec.arguments, i)
		if other_arg.name == arg.name {
			msg := fmt.aprintf(
				"Argument '%v' was already defined in this scope",
				arg.name,
				allocator = eval.allocator,
			)

			exparr_push(eval.errors, Parsing_Error{raw_arg.at, msg})
			return
		} else if .Ambient in other_arg.flags && .Ambient in arg.flags {
			msg := fmt.aprintf(
				"Argument '%v' is ambient, yet '%v' also is",
				other_arg.name,
				arg.name,
				allocator = eval.allocator,
			)

			exparr_push(eval.errors, Parsing_Error{raw_arg.at, msg})
			return
		}
	}

	exparr_push(&partial_spec.arguments, arg)

	return true
}

apt_parse_def :: proc(eval: Evaluator, spec: Apparition_Spec, node: Apt) -> (ok: bool) {
	evaluated_def := apt_parse_apparition_application(
		eval,
		spec,
		node.data.tok,
		node.children,
	) or_return

	name := arg_get_name(eval, spec, node.data.tok, evaluated_def, "name") or_return

	defined_spec: Dynamic_Apparition_Spec = {
		scope = eval.scope,
	}

	defined_spec.arguments.allocator = eval.allocator

	if is_arg_given(spec, evaluated_def, "hoisted") {
		defined_spec.flags += {.Hoisted}
	}

	args := ap_spec_get_arg(spec, evaluated_def, "arg")
	for i in 0 ..< args.len {
		raw_arg := exparr_get(args, i)^
		apt_parse_arg(eval, .Arg, &defined_spec.skeleton, raw_arg) or_continue
	}

	if return_ix, has_return := ap_spec_find_arg(spec, "return"); has_return {
		arg_return := evaluated_def[return_ix]
		defined_spec.body.allocator = eval.allocator
		for i in 0 ..< arg_return.len {
			exparr_push_exparr(&defined_spec.body, exparr_get(arg_return, i).value)
		}
	}

	for i in 0 ..< eval.scope.members.len {
		binding := exparr_get(eval.scope.members, i)
		if binding.name == name {
			msg := fmt.aprintf(
				"Apparition '%v' was already defined in this scope",
				name,
				allocator = eval.allocator,
			)

			exparr_push(eval.errors, Parsing_Error{node.data.tok, msg})
			return
		}
	}

	binding := Binding {
		name = name,
		spec = defined_spec,
	}

	exparr_push(&eval.scope.members, binding)

	return true
}
// }}}
// {{{ Partial apparition parsing
apt_parse :: proc(
	eval: Evaluator,
	spec: Apparition_Spec,
	forest: Varied_Apf,
	arguments: Apparition_Evaluation,
	drop_left_ws := true,
	drop_right_ws := true,
) {
	// Sanity checks
	assert(!ap_spec_is_associative(spec))
	if inner, ok := spec.(Rigid_Apparition_Spec); ok {
		assert(.Quoted not_in inner.flags)
	}

	Passes :: enum {
		Hoisting,
		Ordinary,
	}

	for pass in Passes {
		iter := vapf_iter_mk(forest, drop_left_ws = drop_left_ws, drop_right_ws = drop_right_ws)
		for node in vapf_iter_next(&iter) {
			if node.data.errors.len > 0 {
				exparr_push_exparr(eval.errors, node.data.errors)
				continue
			}

			// Apparition!
			if node.data.kind == .Node {
				switch node.data.tok.content {
				case "--":
					continue
				case "use":
					(pass == .Ordinary) or_continue
					panic("TODO")
				case "def":
					(pass == .Hoisting) or_continue
					apt_parse_def(eval, .Skeleton, node) or_continue
					continue
				case:
					binding := scope_and_argument_lookup(
						spec,
						eval.scope^,
						node.data.tok.content,
					) or_break

					log.infof("found binding %v", binding.name)
					is_hoisted := false
					if inner, ok := binding.spec.(Dynamic_Apparition_Spec); ok {
						is_hoisted = .Hoisted in inner.flags
					}

					((pass == .Hoisting) == is_hoisted) or_continue

					nested_arguments: Apparition_Evaluation

					// TODO: perhaps do not allow querying apparitions that appear
					// multiple times here? Or better said, error-out on them.
					if inner, ok := binding.spec.(Bound_Apparition_Spec); ok {
						for i in 0 ..< inner.instances.len {
							instance := exparr_get(inner.instances, i)
							apt_parse(eval, spec, instance.value, arguments)
						}
					}

					is_associative := ap_spec_is_associative(binding.spec)

					// Detect associative bindings
					if is_associative {
						apt_parse(eval, spec, node.children, arguments, drop_left_ws = false)
					} else if inner, ok := binding.spec.(Rigid_Apparition_Spec);
					   ok && .Quoted in inner.flags {
						// Handled further below
					} else {
						nested_arguments = apt_parse_apparition_application(
							eval,
							binding.spec,
							node.data.tok,
							node.children,
						) or_continue
					}

					switch inner in binding.spec {
					case Primordial_Apparition_Spec, Etternal_Apparition_Spec:
						panic("TODO")
					case Rigid_Apparition_Spec:
						found := false
						iter := ap_spec_arg_iter_mk(spec)
						for arg, i in ap_spec_arg_iter_next(&iter) {
							(arg.name == binding.name) or_continue

							evaluated: Evaluated_Argument
							evaluated.at = node.data.tok
							evaluated.value.allocator = eval.allocator

							if .Quoted in inner.flags {
								iter := vapf_iter_mk(node.children)
								for child in vapf_iter_next(&iter) {
									exparr_push(&evaluated.value, child)
								}
							} else if .Associative in inner.flags {
								// Associative arguments contain no data
							} else {
								// QUESTION: should we handle whitespace here?
								for i in 0 ..< nested_arguments[0].len {
									nested_argument := exparr_get(nested_arguments[0], i)
									exparr_push_exparr(&evaluated.value, nested_argument.value)
								}
							}

							exparr_push(&arguments[i], evaluated)
							found = true
							break
						}

						assert(found, "Impossible! Argument found then disappeared...")
					case Bound_Apparition_Spec: // Already handled!
					case Dynamic_Apparition_Spec:
						body_scope := nest_scope(eval, inner.scope)

						for i in 0 ..< inner.arguments.len {
							arg_binding := Binding {
								name = exparr_get(inner.arguments, i).name,
								spec = Bound_Apparition_Spec{instances = nested_arguments[i]},
							}
							exparr_push(&body_scope.members, arg_binding)
						}

						body_eval := Evaluator {
							internal_allocator = eval.internal_allocator,
							allocator          = eval.allocator,
							errors             = eval.errors,
							scope              = body_scope,
							caller             = eval.scope,
						}

						log.infof("Evaluating body for %v", binding.name)
						for i in 0 ..< inner.body.len {
							apt_parse(body_eval, spec, exparr_get(inner.body, i)^, arguments)
						}
					}

					continue
				}
			}

			// We only handle ambient arguments in the ordinary pass
			if pass == .Hoisting do continue

			ambient_arg, has_ambient := ap_spec_find_ambient_arg(spec)

			if !has_ambient {
				// Error out if we really need an ambient argument
				kind := node.data.tok.kind
				if kind != .Newline && kind != .Space {
					exparr_push(
						eval.errors,
						Parsing_Error {
							node.data.tok,
							"Underlying apparition has no ambient argument",
						},
					)
				}

				continue
			}

			instances := &arguments[ambient_arg]
			log.assertf(
				instances.len == 1,
				"Ambient argument has %v instances (1 expected)",
				instances.len,
			)

			arg := exparr_get(instances^, 0)
			exparr_push(&arg.value, node)

			// TODO: clean up spaces at the start/end of ambient sequences
		}
	}

	return
}
// }}}
// {{{ String evaluation
apt_eval_string :: proc(eval: Evaluator, instances: Evaluated_Argument_Instances) -> string {
	size: uint

	// NOTE(storage): these could really easily be stored on the evaluator
	// (context), since we never need more than one at a time.
	segments: Exparr(Token, 3)
	segments.allocator = eval.internal_allocator

	for i in 0 ..< instances.len {
		instance := exparr_get(instances, i)
		iter := vapf_iter_mk(instance.value)
		for node in vapf_iter_next(&iter) {
			if node.data.kind == .Node {
				exparr_push(
					eval.errors,
					Parsing_Error{node.data.tok, "Unexpected apparition inside string"},
				)

				continue
			}

			tok := node.data.tok

			#partial switch tok.kind {
			case .Word:
				exparr_push(&segments, tok)
				size += len(tok.content)
			case .Space, .Newline:
				// Consecutive spaces are truncated
				if segments.len == 0 || exparr_last(segments).kind == .Space {
					continue
				}

				// Inline newlines are turned into spaces
				tok := Token {
					content = " ",
					kind    = .Space,
					from    = tok.from,
				}

				exparr_push(&segments, tok)
				size += 1
			case:
				panic("Unknown apparition")
			}
		}
	}

	// Allocate builder for the output string. Should never grow past the given
	// size!
	builder := strings.builder_make_len_cap(0, int(size), eval.allocator)

	// Sanity check: attempting to re-allocate the buffer will cause a panic!
	builder.buf.allocator = runtime.panic_allocator()

	for i in 0 ..< segments.len {
		tok := exparr_get(segments, i)
		strings.write_string(&builder, tok.content)
	}

	return strings.to_string(builder)
}
// }}}
// {{{ Argument queries
is_arg_given :: proc(
	spec: Apparition_Spec,
	args: Apparition_Evaluation,
	arg_name: string,
) -> bool {
	return ap_spec_get_arg(spec, args, arg_name).len > 0
}

arg_get_name :: proc(
	eval: Evaluator,
	spec: Apparition_Spec,
	at: Token,
	args: Apparition_Evaluation,
	arg_name: string,
) -> (
	name: string,
	ok: bool,
) {
	raw_name := ap_spec_get_arg(spec, args, arg_name)
	name = apt_eval_string(eval, raw_name)

	if name == "" {
		exparr_push(eval.errors, Parsing_Error{at, "Expected name"})
		return name, false
	} else if name == "def" || name == "--" || name == "use" {
		exparr_push(eval.errors, Parsing_Error{at, "Cannot override fundamental apparition"})
		return name, false
	}

	return name, true
}
// }}}

// Built-in apparitions
// {{{ Helpers
// Helper for turning a slice into an exponential array of arguments.
static_arguments :: proc(slice: ..Apparition_Argument) -> (out: Exparr(Apparition_Argument)) {
	// HACK(storage): this is terrible (but so is re-generating the whole list
	// every time). The "proper" solution would be caching these declarations
	// after generating them once at startup.
	out.allocator = context.allocator
	for v in slice do exparr_push(&out, v)
	return out
}
// }}}
