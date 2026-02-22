#+private file
package anima

import "core:fmt"
import "core:strings"
import "core:terminal/ansi"

// Constants -------------------------------------------------------------------
INDENTATION_INCREMENT :: 2
MAX_LINE_COLS :: 80

FG_RED :: ansi.CSI + ansi.FG_RED + ansi.SGR
FG_GREEN :: ansi.CSI + ansi.FG_GREEN + ansi.SGR
FG_MAGENTA :: ansi.CSI + ansi.FG_MAGENTA + ansi.SGR
ANSI_RESET :: ansi.CSI + ansi.RESET + ansi.SGR

// State manipulation ----------------------------------------------------------
// {{{ Pretty printe state
Markup_Printer_State :: struct {
	// How many spaces should we insert at the start of a new line? Gets
	// automatically increased in the current scope when using mps__deeper. The
	// increments/decrements are currently fixed to INDENTATION_INCREMENT.
	indentation:              uint,
	// The column we're currently at with the text on the current line. Used for
	// wrapping inline elements to MAX_LINE_COLS. Note that we don't currently
	// handle multi-column characters! (at worst, this will make text not wrap
	// right away)
	current_col:              uint,
	// The indentation of the line we're currently pretty printing on.
	current_line_indentation: uint,
	// Two things can be put on the same line if they both allow for it. This
	// keeps track of whether the last chunk of text agreed to the above.
	inlines_allowed_here:     bool,
	output:                   strings.Builder,
}

@(private = "package")
mps__init :: proc() -> (out: Markup_Printer_State) {
	out.output = strings.builder_make_len_cap(0, 1024)
	return out
}

@(private = "package")
mps_destroy :: proc(mps: ^Markup_Printer_State) {
	strings.builder_destroy(&mps.output)
}

// Finalizes the pretty printing by generating a string out of all of it
@(private = "package")
mps__to_string :: proc(mps: Markup_Printer_State) -> string {
	return strings.to_string(mps.output)
}

// The heart of the pretty-printer. Handles creation of nested blocks, and
// decides when to inline them onto the line we're already on.
mps__deeper_raw :: proc(
	mps: ^Markup_Printer_State,
	format: string,
	args: []any,
	allow_inline := false,
	cols: uint = 0,
) {
	str := fmt.tprintf(format, ..args)
	required_inline_cols: uint = 1 + (cols == 0 ? len(str) : cols)

	if allow_inline &&
	   mps.inlines_allowed_here &&
	   mps.current_col + required_inline_cols <= MAX_LINE_COLS &&
	   mps.current_line_indentation == mps.indentation {
		strings.write_rune(&mps.output, ' ')
		fmt.sbprint(&mps.output, str)
		mps.current_col += required_inline_cols
	} else {
		if len(mps__to_string(mps^)) > 0 do strings.write_rune(&mps.output, '\n')
		for i in 0 ..< mps.indentation do strings.write_rune(&mps.output, ' ')
		fmt.sbprint(&mps.output, str)
		mps.current_col = mps.indentation + len(str)
		mps.current_line_indentation = mps.indentation
	}

	mps.indentation += INDENTATION_INCREMENT
	mps.inlines_allowed_here = false
}

// Creates an indented block that ends with the current scope.
@(deferred_out = msp__deeper_end)
mps__deeper :: proc(
	mps: ^Markup_Printer_State,
	name: string,
	allow_inline := false,
) -> ^Markup_Printer_State {
	mps__deeper_raw(
		mps,
		FG_MAGENTA + "%v" + ANSI_RESET,
		{name},
		allow_inline = allow_inline,
		cols = len(name),
	)
	return mps
}

msp__deeper_end :: proc(mps: ^Markup_Printer_State) {
	mps.indentation -= INDENTATION_INCREMENT
}

// Pretty prints a leaf node that's just a string
mps__leaf_str :: proc(mps: ^Markup_Printer_State, inner: any, allow_inline := false) {
	mps__deeper_raw(
		mps,
		FG_GREEN + "%q" + ANSI_RESET,
		{inner},
		allow_inline = allow_inline,
		// There's gotta be a better way...
		cols = len(fmt.tprintf("%q", inner)),
	)

	msp__deeper_end(mps)

	if allow_inline do mps.inlines_allowed_here = true
}

// Pretty prints a leaf node of the form "label: ...str"
mps__leaf_labeled_str :: proc(mps: ^Markup_Printer_State, label: string, inner: any) {
	mps__deeper_raw(
		mps,
		FG_RED + "%v: " + FG_GREEN + "%q" + ANSI_RESET,
		{label, inner},
		cols = len(label) + 2 + len(fmt.tprintf("%q", inner)),
	)

	msp__deeper_end(mps)
}

// Pretty prints a leaf node representing some apparition
mps__leaf :: proc(mps: ^Markup_Printer_State, name: string, allow_inline := false) {
	mps__deeper(mps, name, allow_inline = allow_inline)
	if allow_inline do mps.inlines_allowed_here = true
}
// }}}
// {{{ Helpers for printing other types in one call
@(private = "package")
mps__inline_markup_to_string :: proc(inline_markup: Inline_Markup) -> string {
	mps := mps__init()
	mps__inline_markup(&mps, inline_markup)
	return mps__to_string(mps)
}

// @(private = "package")
// mps_dapf_to_string :: proc(dapf: Exp_Apf) -> string {
// 	mps := mps__init()
// 	mps_distributed_apparition_forest(&mps, dapf)
// 	return strings.to_string(mps.output)
// }
//
// @(private = "package")
// mps_scope_to_string :: proc(scope: Scope) -> string {
// 	mps := mps__init()
// 	mps_scope(&mps, scope)
// 	return strings.to_string(mps.output)
// }
// }}}

// Pretty printers -------------------------------------------------------------
// {{{ Inline markup
mps__inline_markup__atom :: proc(mps: ^Markup_Printer_State, markup: Inline_Markup__Atom) {
	switch inner in markup {
	case nil:
		mps__leaf(mps, "none", allow_inline = true)
	case Inline_Markup__Space:
		mps__leaf(mps, "space", allow_inline = true)
	case Inline_Markup__Ellipsis:
		mps__leaf(mps, "ellipsis", allow_inline = true)
	case Inline_Markup__Text:
		mps__leaf_str(mps, string(inner), allow_inline = true)
	case Inline_Markup__Strong:
		mps__deeper(mps, "strong")
		mps__inline_markup(mps, Inline_Markup(inner))
	case Inline_Markup__Emph:
		mps__deeper(mps, "emph")
		mps__inline_markup(mps, Inline_Markup(inner))
	case Inline_Markup__Strikethrough:
		mps__deeper(mps, "strikethrough")
		mps__inline_markup(mps, Inline_Markup(inner))
	case Inline_Markup__Mono:
		mps__deeper(mps, "mono")
		mps__inline_markup(mps, Inline_Markup(inner))
	case Inline_Markup__Quote:
		mps__deeper(mps, "quote")
		mps__inline_markup(mps, Inline_Markup(inner))
	// case .Fn:
	// 	mps__leaf_labeled_str(mps, "fn", markup.raw)
	// case .Icon:
	// 	mps__leaf_labeled_str(mps, "icon", markup.raw)
	// case .LaTeX:
	// 	mps__leaf_labeled_str(mps, "math", markup.raw)
	// case .Link:
	// 	mps__deeper(mps, "link")
	//
	// 	if markup.link.label.kind != .None {
	// 		mps__deeper(mps, "label")
	// 		mps__inline_markup(mps, markup.link.label^)
	// 	}
	//
	// 	mps__leaf_labeled_str(mps, "id", markup.link.id)
	// case .Date:
	// 	mps__deeper(mps, "date")
	// 	if markup.time.compact do mps__leaf(mps, "compact")
	// 	mps__leaf_str(mps, markup.time.time)
	// case .Datetime:
	// 	mps__deeper(mps, "datetime")
	// 	if markup.time.compact do mps__leaf(mps, "compact")
	// 	mps__leaf_str(mps, markup.time.time)
	case:
		mps__leaf(mps, "unknown", allow_inline = true)
	}
}

mps__inline_markup :: proc(mps: ^Markup_Printer_State, markup: Inline_Markup) {
	for i in 0 ..< markup.elements.len {
		inner := exparr__get(markup.elements, i)
		mps__inline_markup__atom(mps, inner^)
	}
}
// }}}
// // {{{ Block markup
// @(private = "package")
// mps_block_markup :: proc(mps: ^Markup_Printer_State, markup: Block_Markup) {
// 	switch markup.kind {
// 	case .None:
// 		mps__leaf(mps, "none")
// 	case .Thematic_Break:
// 		mps__leaf(mps, "thematic-break")
// 	case .Table_Of_Contents:
// 		mps__leaf(mps, "table-of-contents")
// 	case .Embed_Description:
// 		mps__leaf(mps, "embed-description")
// 	case .Paragraph:
// 		mps__deeper(mps, "paragraph")
// 		mps__inline_markup(mps, markup.paragraph^)
// 	case .Blockquote:
// 		mps__deeper(mps, "blockquote")
// 		mps_block_markup(mps, markup.blockquote^)
// 	case .Heading:
// 		mps__deeper(mps, "heading")
// 		mps__leaf_labeled_str(mps, "level", fmt.tprint(markup.heading.level))
//
// 		if markup.heading.id != {} {
// 			mps__leaf_labeled_str(mps, "id", markup.heading.id)
// 		}
//
// 		mps__inline_markup(mps, markup.heading.contents)
// 	case .Image:
// 		mps__deeper(mps, "image")
// 		mps__leaf_labeled_str(mps, "source", markup.image.source)
// 		mps__deeper(mps, "alt")
// 		mps__inline_markup(mps, markup.image.alt^)
// 	case .Figure:
// 		mps__deeper(mps, "figure")
//
// 		{
// 			mps__deeper(mps, "caption")
// 			mps__inline_markup(mps, markup.figure.caption^)
// 		}
//
// 		mps_block_markup(mps, markup.figure.content^)
// 	case .Aside:
// 		mps__deeper(mps, "aside")
//
// 		character := markup.aside.character
// 		if markup.aside.collapse do mps__leaf(mps, "collapse")
// 		if markup.aside.id != {} do mps__leaf_labeled_str(mps, "id", markup.aside.id)
// 		if character != {} do mps__leaf_labeled_str(mps, "character", character)
//
// 		{
// 			mps__deeper(mps, "title")
// 			mps__inline_markup(mps, markup.aside.title^)
// 		}
//
// 		mps_block_markup(mps, markup.aside.content^)
// 	case .Linkdef:
// 		mps__deeper(mps, "linkdef")
// 		mps__leaf_labeled_str(mps, "id", markup.linkdef.id)
//
// 		if markup.linkdef.label.kind != .None {
// 			mps__deeper(mps, "label")
// 			mps__inline_markup(mps, markup.linkdef.label)
// 		}
//
// 		mps__leaf_str(mps, markup.linkdef.target)
// 	case .Fndef:
// 		mps__deeper(mps, "fndef")
// 		mps__leaf_labeled_str(mps, "id", markup.fndef.id)
// 		mps_block_markup(mps, markup.fndef.contents)
// 	case .Codeblock:
// 		mps__deeper(mps, "codeblock")
//
// 		if markup.codeblock.lang != {} {
// 			mps__leaf_labeled_str(mps, "lang", markup.codeblock.lang)
// 		}
//
// 		mps__leaf_str(mps, markup.codeblock.source)
// 	case .Page_Index:
// 		mps__deeper(mps, "page-index")
// 		mps_page_filters(mps, markup.page_index.filters)
// 	case .Table:
// 		mps__deeper(mps, "table")
//
// 		if markup.table.caption.kind != .None {
// 			mps__deeper(mps, "caption")
// 			mps__inline_markup(mps, markup.table.caption^)
// 		}
//
// 		if markup.table.head.cells.len > 0 {
// 			mps__deeper(mps, "head")
// 			mps_table_row(mps, markup.table.head^)
// 		}
//
// 		for i in 0 ..< markup.table.rows.len {
// 			inner := exparr_get(markup.table.rows, i)
// 			mps__deeper(mps, "row")
// 			mps_table_row(mps, markup.table.head^)
// 		}
// 	case .IList:
// 		mps__deeper(mps, "list")
// 		if markup.ilist.ordered do mps__leaf(mps, "ordered")
//
// 		for i in 0 ..< markup.ilist.items.len {
// 			item := exparr_get(markup.ilist.items, i)
// 			mps__deeper(mps, "item")
// 			mps__inline_markup(mps, item^)
// 		}
// 	case .BList:
// 		mps__deeper(mps, "list")
// 		if markup.blist.ordered do mps__leaf(mps, "ordered")
//
// 		for i in 0 ..< markup.blist.items.len {
// 			item := exparr_get(markup.blist.items, i)
// 			mps__deeper(mps, "item")
// 			mps_block_markup(mps, item^)
// 		}
// 	case .Many:
// 		for i in 0 ..< markup.many.len {
// 			inner := exparr_get(markup.many, i)
// 			mps_block_markup(mps, inner^)
// 		}
// 	case:
// 		mps__leaf(mps, "unknown")
// 	}
// }
//
// mps_table_row :: proc(mps: ^Markup_Printer_State, row: Table_Row) {
// 	for i in 0 ..< row.cells.len {
// 		cell := exparr_get(row.cells, i)
// 		mps__deeper(mps, "cell")
// 		if cell.bg != {} do mps__leaf_labeled_str(mps, "bg", cell.bg)
// 		mps__inline_markup(mps, cell.content^)
// 	}
// }
//
// mps_page_filters :: proc(mps: ^Markup_Printer_State, filters: Page_Filters) {
// 	if filters.hidden do mps__leaf(mps, "hidden")
//
// 	if filters.children != {} {
// 		mps__leaf_labeled_str(mps, "children", filters.children)
// 	}
//
// 	if filters.descendants != {} {
// 		mps__leaf_labeled_str(mps, "descendants", filters.descendants)
// 	}
// }
// // }}}
// // {{{ Apparition trees
// mps_apparition_tree :: proc(mps: ^Markup_Printer_State, node: Apt_Old) {
// 	switch node.data.kind {
// 	case .Node:
// 		mps__deeper(mps, node.data.tok.content)
// 		if node.data.errors.len == 0 {
// 			mps_apparition_forest(mps, node.children)
// 		} else {
// 			for i in 0 ..< node.data.errors.len {
// 				err := exparr_get(node.data.errors, i)
// 				mps__leaf_labeled_str(mps, "error", err.msg)
// 			}
// 			mps__leaf_labeled_str(mps, "raw", node.data.raw)
// 		}
// 	case .Leaf:
// 		#partial switch node.data.tok.kind {
// 		case .Space:
// 			mps__leaf(mps, "space", allow_inline = true)
// 		case .Newline:
// 			mps__leaf(mps, "newline", allow_inline = true)
// 		case .Word:
// 			mps__leaf_str(mps, node.data.tok.content, allow_inline = true)
// 		case:
// 			fmt.panicf("Invalid apt node %v", node.data.tok.kind)
// 		}
// 	}
// }
//
// @(private = "package")
// mps_apparition_forest :: proc(mps: ^Markup_Printer_State, cl: Linked_Apf) {
// 	for next := cl.next; next != nil; next = next.siblings.next {
// 		mps_apparition_tree(mps, next^)
// 	}
// }
//
// @(private = "package")
// mps_distributed_apparition_forest :: proc(mps: ^Markup_Printer_State, dapf: Exp_Apf) {
// 	iter := vapf_iter_mk(dapf)
// 	for node in vapf_iter_next(&iter) {
// 		mps_apparition_tree(mps, node)
// 	}
// }
// // }}}
// // {{{ Tokens
// // Prints every token in a file
// @(private = "package")
// mps_tokens :: proc(mps: ^Markup_Printer_State, source: string) {
// 	arena: virtual.Arena
// 	err := virtual.arena_init_growing(&arena)
// 	if err != .None do log.panicf("Virtual arena init error: %v", err)
// 	defer virtual.arena_destroy(&arena)
//
// 	allocator := virtual.arena_allocator(&arena)
// 	lexer, ok := mk_lexer(source, allocator = allocator)
//
// 	if ok {
// 		for {
// 			tok := tokenize(&lexer) or_break
// 			switch tok.kind {
// 			case .Bang, .Colon, .LCurly, .RCurly, .Word:
// 				mps__leaf_str(mps, tok.content, allow_inline = true)
// 			case .None:
// 				mps__leaf(mps, "none", allow_inline = true)
// 			case .Eof:
// 				mps__leaf(mps, "eof", allow_inline = true)
// 			case .Newline:
// 				mps__leaf(mps, "newline", allow_inline = true)
// 			case .Space:
// 				mps__leaf(mps, "space", allow_inline = true)
// 			case .Apparition:
// 				mps__leaf(
// 					mps,
// 					fmt.aprintf("\\%v", tok.content, allocator = allocator),
// 					allow_inline = true,
// 				)
// 			}
// 			if tok.kind == .Eof do break
// 		}
// 	}
//
// 	if lexer.error != {} {
// 		mps__deeper(mps, "error")
// 		mps__leaf_str(mps, lexer.error.msg)
//
// 		pos := fmt.aprintf(
// 			"%v:%v",
// 			lexer.error.pos.line,
// 			lexer.error.pos.col,
// 			allocator = allocator,
// 		)
// 		mps__leaf_labeled_str(mps, "loc", pos)
// 	}
// }
// // }}}
// // {{{ Scopes
// @(private = "package")
// mps_scope :: proc(mps: ^Markup_Printer_State, scope: Scope) {
// 	for i in 0 ..< scope.members.len {
// 		member := exparr_get(scope.members, i)
// 		mps__leaf_str(mps, member.name)
// 	}
//
// 	if scope.parent != nil {
// 		mps__deeper(mps, "parent")
// 		mps_scope(mps, scope.parent^)
// 	}
// }
// // }}}
