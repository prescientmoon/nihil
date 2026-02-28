#+private file
package anima

import "core:fmt"
import "core:log"
import "core:mem/virtual"
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

@(private = "package")
mps__block_markup_to_string :: proc(block_markup: Block_Markup) -> string {
	mps := mps__init()
	mps__block_markup(&mps, block_markup)
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
// {{{ Contiguous text
mps__contiguous_text :: proc(mps: ^Markup_Printer_State, text: Contiguous_Text) {
	if text.len == 0 do mps__leaf_str(mps, "", allow_inline = true)
	for i in 0 ..< text.len {
		chunk := exparr__get(text, i)
		mps__leaf_str(mps, chunk^, allow_inline = true)
	}
}
// }}}
// {{{ Inline markup
mps__inline_markup__atom :: proc(mps: ^Markup_Printer_State, markup: Inline_Markup__Atom) {
	switch inner in markup {
	case nil:
		mps__leaf(mps, "inline›none", allow_inline = true)
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
	case Inline_Markup__Icon:
		// NOTE: these should ideally use mps__leaf_labeled_str, except that doesn't
		// currently accept Contiguous_Text...
		mps__deeper(mps, "icon")
		mps__contiguous_text(mps, Contiguous_Text(inner))
	case Inline_Markup__Fn:
		mps__deeper(mps, "fn")
		mps__contiguous_text(mps, Contiguous_Text(inner))
	case Inline_Markup__Link:
		mps__deeper(mps, "link")
		mps__contiguous_text(mps, inner.id)
		if inner.label.elements.len != 0 {
			mps__deeper(mps, "label")
			mps__inline_markup(mps, inner.label)
		}
	// case .LaTeX:
	// 	mps__leaf_labeled_str(mps, "math", markup.raw)
	// cps__leaf_labeled_str(mps, "id", markup.link.id)
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
// {{{ Block markup
mps__block_markup__atom :: proc(mps: ^Markup_Printer_State, markup: Block_Markup__Atom) {
	switch inner in markup {
	case nil:
		mps__leaf(mps, "block›none", allow_inline = true)
	case Block_Markup__Paragraph:
		mps__deeper(mps, "paragraph")
		mps__inline_markup(mps, Inline_Markup(inner))
	case Block_Markup__Blockquote:
		mps__deeper(mps, "blockquote")
		mps__block_markup(mps, Block_Markup(inner))
	case Block_Markup__Image:
		mps__deeper(mps, "image")
		{mps__deeper(mps, "source"); mps__contiguous_text(mps, inner.source)}
		{mps__deeper(mps, "alt"); mps__inline_markup(mps, inner.alt)}
	case Block_Markup__Description:
		mps__leaf(mps, "embed-description")
	case Block_Markup__Table_Of_Contents:
		mps__leaf(mps, "table-of-contents")
	case Block_Markup__Thematic_Break:
		mps__leaf(mps, "thematic-break")
	case Block_Markup__Figure:
		mps__deeper(mps, "figure")
		{mps__deeper(mps, "caption"); mps__inline_markup(mps, inner.caption)}
		mps__block_markup(mps, inner.content)
	case ^Linkdef:
		mps__deeper(mps, "linkdef")
		{mps__deeper(mps, "id"); mps__contiguous_text(mps, inner.id)}
		{mps__deeper(mps, "target"); mps__contiguous_text(mps, inner.target)}
    if inner.label.elements.len != 0 do mps__inline_markup(mps, inner.label)
	case ^Fndef:
		mps__deeper(mps, "fndef")
		{mps__deeper(mps, "id"); mps__contiguous_text(mps, inner.id)}
    mps__block_markup(mps, inner.content)
	case Table:
		mps__deeper(mps, "table")

		if inner.caption.elements.len != 0 {
			mps__deeper(mps, "caption")
			mps__inline_markup(mps, inner.caption)
		}

		mps__table_cell :: proc(mps: ^Markup_Printer_State, cell: Table__Cell) {
			mps__inline_markup(mps, cell.content)
		}

		mps__table_row :: proc(mps: ^Markup_Printer_State, row: Table__Row) {
			for i in 0 ..< row.cells.len {
				cell := exparr__get(row.cells, i)^
				mps__deeper(mps, "cell")
				mps__table_cell(mps, cell)
			}
		}

		{mps__deeper(mps, "header"); mps__table_row(mps, inner.header)}

		for i in 0 ..< inner.rows.len {
			row := exparr__get(inner.rows, i)^
			mps__deeper(mps, "row")
			mps__table_row(mps, row)
		}
	case Block_Markup__List:
		mps__deeper(mps, "list")

		if inner.ordered do mps__leaf(mps, "ordered")
		else do mps__leaf(mps, "unordered")
		if inner.block do mps__leaf(mps, "block")
		else do mps__leaf(mps, "inline")

		switch elements in inner.elements {
		case Exparr(Inline_Markup):
			for i in 0 ..< elements.len {
				mps__deeper(mps, "item")
				mps__inline_markup(mps, exparr__get(elements, i)^)
			}
		case Exparr(Block_Markup):
			for i in 0 ..< elements.len {
				mps__deeper(mps, "item")
				mps__block_markup(mps, exparr__get(elements, i)^)
			}
		}
	case:
		mps__leaf(mps, "unknown", allow_inline = true)
	}
}

mps__block_markup :: proc(mps: ^Markup_Printer_State, markup: Block_Markup) {
	for i in 0 ..< markup.elements.len {
		inner := exparr__get(markup.elements, i)
		mps__block_markup__atom(mps, inner^)
	}
}
// }}}
// {{{ Tokens
// Prints every token in a file
@(private = "package")
mps_tokens :: proc(mps: ^Markup_Printer_State, source: string) {
	arena: virtual.Arena
	err := virtual.arena_init_growing(&arena)
	log.assert(err == nil)
	defer virtual.arena_destroy(&arena)

	lexer, ok := lexer__make(source, &arena)
	allocator := virtual.arena_allocator(&arena)

	if ok {
		for {
			tok := tokenize(&lexer) or_break
			switch tok.kind {
			case .Bang, .LCurly, .RCurly, .Word:
				mps__leaf_str(mps, tok.content, allow_inline = true)
			case .None:
				mps__leaf(mps, "token›none", allow_inline = true)
			case .Eof:
				mps__leaf(mps, "eof", allow_inline = true)
			case .Newline:
				mps__leaf(mps, "newline", allow_inline = true)
			case .Space:
				mps__leaf(mps, "space", allow_inline = true)
			case .Apparition:
				mps__leaf(
					mps,
					fmt.aprintf("\\%v", tok.content, allocator = allocator),
					allow_inline = true,
				)
			}
			if tok.kind == .Eof do break
		}
	}

	if lexer.error != {} {
		mps__deeper(mps, "error")
		mps__leaf_str(mps, lexer.error.msg)

		pos := fmt.aprintf(
			"%v:%v",
			lexer.error.pos.line,
			lexer.error.pos.col,
			allocator = allocator,
		)

		mps__leaf_labeled_str(mps, "loc", pos)
	}
}
// }}}
