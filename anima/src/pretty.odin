#+private file
package anima

import "core:fmt"
import "core:strings"
import "core:terminal/ansi"
import "core:time"

// Constants -------------------------------------------------------------------
INDENTATION_INCREMENT :: 2
MAX_LINE_COLS :: 80

@(private = "file")
FG_RED :: ansi.CSI + ansi.FG_RED + ansi.SGR

@(private = "file")
FG_GREEN :: ansi.CSI + ansi.FG_GREEN + ansi.SGR

@(private = "file")
FG_MAGENTA :: ansi.CSI + ansi.FG_MAGENTA + ansi.SGR

@(private = "file")
ANSI_RESET :: ansi.CSI + ansi.RESET + ansi.SGR

// State manipulation ----------------------------------------------------------
Markup_Printer_State :: struct {
	// How many spaces should we insert at the start of a new line? Gets
	// automatically increased in the current scope when using mps_deeper. The
	// increments/decrements are currently fixed to INDENTATION_INCREMENT.
	indentation:          uint,
	// The column we're currently at with the text on the current line. Used for
	// wrapping inline elements to MAX_LINE_COLS. Note that we don't currently
	// handle multi-column characters! (at worst, this will make text not wrap
	// right away)
	current_col:          uint,
	// Two things can be put on the same line if they both allow for it. This
	// keeps track of whether the last chunk of text agreed to the above.
	inlines_allowed_here: bool,
	output:               strings.Builder,
}

@(private = "package")
mps_init :: proc() -> (out: Markup_Printer_State) {
	out.output = strings.builder_make_len_cap(0, 1024)
	return out
}

@(private = "package")
mps_to_string :: proc(mps: Markup_Printer_State) -> string {
	return strings.to_string(mps.output)
}

mps_deeper_raw :: proc(
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
	   mps.current_col + required_inline_cols <= MAX_LINE_COLS {
		strings.write_rune(&mps.output, ' ')
		fmt.sbprint(&mps.output, str)
		mps.current_col += required_inline_cols
	} else {
		if len(mps_to_string(mps^)) > 0 do strings.write_rune(&mps.output, '\n')
		for i in 0 ..< mps.indentation do strings.write_rune(&mps.output, ' ')
		fmt.sbprint(&mps.output, str)
		mps.current_col = mps.indentation + len(str)
	}

	mps.indentation += INDENTATION_INCREMENT
	mps.inlines_allowed_here = false
}

@(deferred_out = mps_deeper_end)
mps_deeper :: proc(
	mps: ^Markup_Printer_State,
	name: string,
	allow_inline := false,
) -> ^Markup_Printer_State {
	mps_deeper_raw(
		mps,
		FG_MAGENTA + "%v" + ANSI_RESET,
		{name},
		allow_inline = allow_inline,
		cols = len(name),
	)
	return mps
}

mps_deeper_end :: proc(mps: ^Markup_Printer_State) {
	mps.indentation -= INDENTATION_INCREMENT
}

// Pretty prints a leaf node that's just a string
mps_leaf_str :: proc(mps: ^Markup_Printer_State, inner: any, allow_inline := false) {
	mps_deeper_raw(
		mps,
		FG_GREEN + "%q" + ANSI_RESET,
		{inner},
		allow_inline = allow_inline,
		// There's gotta be a better way...
		cols = len(fmt.tprintf("%q", inner)),
	)

	mps_deeper_end(mps)

	if allow_inline {
		mps.inlines_allowed_here = true
	}
}

// Pretty prints a leaf node of the form "label: ...str"
mps_leaf_labeled_str :: proc(mps: ^Markup_Printer_State, label: string, inner: any) {
	mps_deeper_raw(
		mps,
		FG_RED + "%v: " + FG_GREEN + "%q" + ANSI_RESET,
		{label, inner},
		cols = len(label) + 2 + len(fmt.tprintf("%q", inner)),
	)

	mps_deeper_end(mps)
}

// Pretty prints a leaf node representing some apparition
mps_leaf :: proc(mps: ^Markup_Printer_State, name: string, allow_inline := false) {
	mps_deeper(mps, name, allow_inline = allow_inline)
}

// Pretty printers -------------------------------------------------------------
mps_inline_markup :: proc(mps: ^Markup_Printer_State, markup: Inline_Markup) {
	switch markup.kind {
	case .Space:
	// Nothing :3
	case .None:
		mps_leaf(mps, "none")
	case .Ellipsis:
		mps_leaf(mps, "ellipsis")
	case .Text:
		mps_leaf_str(mps, markup.raw, allow_inline = true)
	case .Fn:
		mps_leaf_labeled_str(mps, "fn", markup.raw)
	case .Icon:
		mps_leaf_labeled_str(mps, "icon", markup.raw)
	case .Strong:
		mps_deeper(mps, "strong")
		mps_inline_markup(mps, markup.inner^)
	case .Emph:
		mps_deeper(mps, "emph")
		mps_inline_markup(mps, markup.inner^)
	case .Strikethrough:
		mps_deeper(mps, "strikethrough")
		mps_inline_markup(mps, markup.inner^)
	case .Mono:
		mps_deeper(mps, "mono")
		mps_inline_markup(mps, markup.inner^)
	case .Quote:
		mps_deeper(mps, "quote")
		mps_inline_markup(mps, markup.inner^)
	case .LaTeX:
		mps_leaf_labeled_str(mps, "math", markup.raw)
	case .Link:
		mps_deeper(mps, "link")

		if markup.link.label.kind != .None {
			mps_deeper(mps, "label")
			mps_inline_markup(mps, markup.link.label^)
		}

		mps_leaf_labeled_str(mps, "id", markup.link.id)
	case .Date:
		mps_deeper(mps, "date")
		if markup.time.compact do mps_leaf(mps, "compact")
		mps_leaf_str(mps, markup.time.time)
	case .Datetime:
		mps_deeper(mps, "datetime")
		if markup.time.compact do mps_leaf(mps, "compact")
		mps_leaf_str(mps, markup.time.time)
	case .Many:
		for i in 0 ..< markup.many.len {
			inner := exparr_get(markup.many, i)
			mps_inline_markup(mps, inner^)
		}
	case:
		mps_leaf(mps, "unknown")
	}
}

@(private = "package")
mps_block_markup :: proc(mps: ^Markup_Printer_State, markup: Block_Markup) {
	switch markup.kind {
	case .None:
		mps_leaf(mps, "none")
	case .Thematic_Break:
		mps_leaf(mps, "thematic-break")
	case .Table_Of_Contents:
		mps_leaf(mps, "table-of-contents")
	case .Embed_Description:
		mps_leaf(mps, "embed-description")
	case .Paragraph:
		mps_deeper(mps, "paragraph")
		mps_inline_markup(mps, markup.paragraph^)
	case .Blockquote:
		mps_deeper(mps, "blockquote")
		mps_block_markup(mps, markup.blockquote^)
	case .Heading:
		mps_deeper(mps, "heading")
		mps_leaf_labeled_str(mps, "level", fmt.tprint(markup.heading.level))

		if markup.heading.id != {} {
			mps_leaf_labeled_str(mps, "id", markup.heading.id)
		}

		mps_inline_markup(mps, markup.heading.contents)
	case .Image:
		mps_deeper(mps, "image")
		mps_leaf_labeled_str(mps, "source", markup.image.source)
		mps_deeper(mps, "alt")
		mps_inline_markup(mps, markup.image.alt^)
	case .Figure:
		mps_deeper(mps, "figure")

		{
			mps_deeper(mps, "caption")
			mps_inline_markup(mps, markup.figure.caption^)
		}

		mps_block_markup(mps, markup.figure.content^)
	case .Aside:
		mps_deeper(mps, "aside")

		character := markup.aside.character
		if markup.aside.collapse do mps_leaf(mps, "collapse")
		if markup.aside.id != {} do mps_leaf_labeled_str(mps, "id", markup.aside.id)
		if character != {} do mps_leaf_labeled_str(mps, "character", character)

		{
			mps_deeper(mps, "title")
			mps_inline_markup(mps, markup.aside.title^)
		}

		mps_block_markup(mps, markup.aside.content^)
	case .Linkdef:
		mps_deeper(mps, "linkdef")
		mps_leaf_labeled_str(mps, "id", markup.linkdef.id)

		if markup.linkdef.label.kind != .None {
			mps_deeper(mps, "label")
			mps_inline_markup(mps, markup.linkdef.label)
		}

		mps_leaf_str(mps, markup.linkdef.target)
	case .Fndef:
		mps_deeper(mps, "fndef")
		mps_leaf_labeled_str(mps, "id", markup.fndef.id)
		mps_block_markup(mps, markup.fndef.contents)
	case .Codeblock:
		mps_deeper(mps, "codeblock")

		if markup.codeblock.lang != {} {
			mps_leaf_labeled_str(mps, "lang", markup.codeblock.lang)
		}

		mps_leaf_str(mps, markup.codeblock.source)
	case .Page_Index:
		mps_deeper(mps, "page-index")
		mps_page_filters(mps, markup.page_index.filters)
	case .Table:
		mps_deeper(mps, "table")

		if markup.table.caption.kind != .None {
			mps_deeper(mps, "caption")
			mps_inline_markup(mps, markup.table.caption^)
		}

		if markup.table.head.cells.len > 0 {
			mps_deeper(mps, "head")
			mps_table_row(mps, markup.table.head^)
		}

		for i in 0 ..< markup.table.rows.len {
			inner := exparr_get(markup.table.rows, i)
			mps_deeper(mps, "row")
			mps_table_row(mps, markup.table.head^)
		}
	case .IList:
		mps_deeper(mps, "list")
		if markup.ilist.ordered do mps_leaf(mps, "ordered")

		for i in 0 ..< markup.ilist.items.len {
			item := exparr_get(markup.ilist.items, i)
			mps_deeper(mps, "item")
			mps_inline_markup(mps, item^)
		}
	case .BList:
		mps_deeper(mps, "list")
		if markup.blist.ordered do mps_leaf(mps, "ordered")

		for i in 0 ..< markup.blist.items.len {
			item := exparr_get(markup.blist.items, i)
			mps_deeper(mps, "item")
			mps_block_markup(mps, item^)
		}
	case .Many:
		for i in 0 ..< markup.many.len {
			inner := exparr_get(markup.many, i)
			mps_block_markup(mps, inner^)
		}
	case:
		mps_leaf(mps, "unknown")
	}
}

mps_table_row :: proc(mps: ^Markup_Printer_State, row: Table_Row) {
	for i in 0 ..< row.cells.len {
		cell := exparr_get(row.cells, i)
		mps_deeper(mps, "cell")
		if cell.bg != {} do mps_leaf_labeled_str(mps, "bg", cell.bg)
		mps_inline_markup(mps, cell.content^)
	}
}

mps_page_filters :: proc(mps: ^Markup_Printer_State, filters: Page_Filters) {
	if filters.hidden do mps_leaf(mps, "hidden")

	if filters.children != {} {
		mps_leaf_labeled_str(mps, "children", filters.children)
	}

	if filters.descendants != {} {
		mps_leaf_labeled_str(mps, "descendants", filters.descendants)
	}
}
