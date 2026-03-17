// A somewhat scuffed module for pretty printing trees of things. Right now all
// allocations reference the temporary allocator, although I might change this
// to use a dedicated arena in the future.
#+private file
package anima

import "core:time"
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
	out.output = strings.builder_make_len_cap(0, 1024, context.temp_allocator)
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
		for _ in 0 ..< mps.indentation do strings.write_rune(&mps.output, ' ')
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
mps__leaf_str :: proc(
  mps: ^Markup_Printer_State, inner: any, allow_inline := false
) {
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
mps__labeled_str :: proc(
  mps: ^Markup_Printer_State, label: string, inner: string
) {
	mps__deeper_raw(
		mps,
		FG_RED + "%v: " + FG_GREEN + "%q" + ANSI_RESET,
		{label, inner},
		cols = len(label) + 2 + len(fmt.tprintf("%q", inner)),
	)

	msp__deeper_end(mps)
}

// Pretty prints a leaf node representing some apparition
mps__leaf :: proc(
  mps: ^Markup_Printer_State, name: string, allow_inline := false
) {
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

@(private = "package")
mps__page_to_string :: proc(page: Page) -> string {
	mps := mps__init()
	mps__page(&mps, page)
	return mps__to_string(mps)
}
// }}}

// Pretty printers -------------------------------------------------------------
// {{{ Timestamp
mps__labeled_timestamp :: proc(
  mps: ^Markup_Printer_State, label: string, timestamp: time.Time
) {
  str := fmt.tprintf("%v", timestamp)
  mps__labeled_str(mps, label, str)
}
// }}}
// {{{ Inline markup
mps__inline_markup__atom :: proc(
  mps: ^Markup_Printer_State, markup: Inline_Markup__Atom
) {
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
	case ^Inline_Markup__Icon:
    mps__labeled_str(mps, "icon", inner.id)
	case ^Inline_Markup__Fn:
    mps__labeled_str(mps, "fn", inner.id)
	case ^Inline_Markup__Link:
		mps__deeper(mps, "link")
		mps__leaf_str(mps, inner.id)
		if inner.label.elements.len != 0 {
			mps__deeper(mps, "label")
			mps__inline_markup(mps, inner.label)
		}
	case Inline_Markup__Date:
		mps__deeper(mps, "date")
    if inner.compact do mps__leaf(mps, "compact")
    mps__leaf_str(mps, fmt.tprintf("%v", inner.time))
	case Inline_Markup__Datetime:
		mps__deeper(mps, "datetime")
    if inner.compact do mps__leaf(mps, "compact")
    mps__leaf_str(mps, fmt.tprintf("%v", inner.time))
	// case .LaTeX:
	// 	mps__labeled_str(mps, "math", markup.raw)
	case:
		mps__leaf(mps, "unknown", allow_inline = true)
	}
}

mps__inline_markup :: proc(mps: ^Markup_Printer_State, markup: Inline_Markup) {
	for i in 0 ..< markup.elements.len {
		inner := exparr__get(markup.elements^, i)
		mps__inline_markup__atom(mps, inner^)
	}
}
// }}}
// {{{ Block markup
mps__block_markup__atom :: proc(
  mps: ^Markup_Printer_State, markup: Block_Markup__Atom
) {
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

    mps__labeled_str(mps, "source", inner.source)

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
	case ^Def__Link:
		mps__deeper(mps, "deflink")

    mps__labeled_str(mps, "id", inner.id)
    mps__labeled_str(mps, "target", inner.target)

    if inner.label.elements.len != 0 do mps__inline_markup(mps, inner.label)
	case ^Def__Icon:
		mps__deeper(mps, "deficon")

    mps__labeled_str(mps, "id", inner.id)
    mps__labeled_str(mps, "path", inner.path)

		mps__deeper(mps, "scope")
    mps__page_filter__many(mps, inner.scope)
	case ^Def__Footnote:
		mps__deeper(mps, "defnote")

    mps__labeled_str(mps, "id", inner.id)
    mps__block_markup(mps, inner.content)
	case Block_Markup__Index:
		mps__deeper(mps, "index")
    mps__page_filter__many(mps, Page_Filter__All(inner))
	case Block_Markup__Code:
		mps__leaf(mps, "code")
	case Block_Markup__Aside:
		mps__deeper(mps, "aside")

		if inner.collapse do mps__leaf(mps, "collapse")

		if inner.id != {} {
      mps__labeled_str(mps, "id", inner.id)
    }

		if inner.char != {} {
      mps__labeled_str(mps, "char", inner.char)
    }

		if inner.title.elements.len > 0 {
      mps__deeper(mps, "title")
      mps__inline_markup(mps, inner.title)
    }

    mps__block_markup(mps, inner.content)
	case ^Heading:
		mps__deeper(mps, "heading")
		mps__labeled_str(mps, "level", fmt.tprint(inner.level))

		if inner.id != {} {
      mps__deeper(mps, "id")
      mps__leaf_str(mps, inner.id)
    }

    mps__inline_markup(mps, inner.content)
	case Block_Markup__Section:
		mps__deeper(mps, "section")
    mps__block_markup__atom(mps, inner.heading)
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

    if inner.block {
			for i in 0 ..< inner.bmarkup.len {
				mps__deeper(mps, "item")
				mps__block_markup(mps, exparr__get(inner.bmarkup, i)^)
			}
    } else {
			for i in 0 ..< inner.imarkup.len {
				mps__deeper(mps, "item")
				mps__inline_markup(mps, exparr__get(inner.imarkup, i)^)
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
mps_tokens :: proc(mps: ^Markup_Printer_State, file: ^File) {
	lexer, ok := lexer__make(file, context.temp_allocator)

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
					fmt.tprintf("\\%v", tok.content),
					allow_inline = true,
				)
			}
			if tok.kind == .Eof do break
		}
	}

	if lexer.error != {} {
		mps__deeper(mps, "error")
		mps__leaf_str(mps, lexer.error.msg)

		pos := fmt.tprintf(
			"%v:%v",
			lexer.error.pos.line,
			lexer.error.pos.col,
		)

		mps__labeled_str(mps, "loc", pos)
	}
}
// }}}
// {{{ Filters
mps__page_filter__many :: proc(
  mps: ^Markup_Printer_State, many: $T/Page_Filter__Many
) {
  for i in 0..<many.elements.len {
    mps__page_filter__atom(mps, exparr__get(many.elements, i)^)
  }
}

mps__page_filter__atom :: proc(
  mps: ^Markup_Printer_State, atom: Page_Filter__Atom
) {
  switch inner in atom {
  case Page_Filter__Local: mps__leaf(mps, "local")
  case Page_Filter__Public: mps__leaf(mps, "public")
  case Page_Filter__Not:
    mps__deeper(mps, "not")
    mps__page_filter__atom(mps, inner^)
  case Page_Filter__All:
    mps__deeper(mps, "all")
    mps__page_filter__many(mps, inner)
  case Page_Filter__Any:
    mps__deeper(mps, "any")
    mps__page_filter__many(mps, inner)
  case Page_Filter__Tag:
    mps__labeled_str(mps, "tag", string(inner))
  }
}
// }}}
// {{{ Page
mps__page :: proc(mps: ^Markup_Printer_State, page: Page) {
  timestamp :: mps__labeled_timestamp
  str :: mps__labeled_str

  mps__deeper(mps, "page")

  if page.compact do mps__leaf(mps, "compact")
  if page.public do mps__leaf(mps, "public")
  else do mps__leaf(mps, "private")

  if page.created_at   != {} do timestamp(mps, "created",   page.created_at)
  if page.published_at != {} do timestamp(mps, "published", page.published_at)

  if mem__nz(page.filename)   do str(mps, "filename",   page.filename)
  if mem__nz(page.priority)   do str(mps, "priority",   page.priority)
  if mem__nz(page.changefreq) do str(mps, "changefreq", page.changefreq)

  if mem__nz(page.title) {
    mps__deeper(mps, "title")
    mps__inline_markup(mps, page.title)
  }

  if mem__nz(page.description) {
    mps__deeper(mps, "description")
    mps__inline_markup(mps, page.description)
  }

  for i in 0..<page.feeds.len {
    mps__feed(mps, exparr__get(page.feeds, i)^)
  }

  for i in 0..<page.changelog.len {
    change := exparr__get(page.changelog, i)^
    mps__deeper(mps, "change")
    mps__labeled_timestamp(mps, "at", change.at)
    mps__inline_markup(mps, change.message)
  }

  for i in 0..<page.tags.len {
    tag := string(exparr__get(page.tags, i)^)
    mps__labeled_str(mps, "tag", tag)
  }

  for i in 0..<page.aliases.len {
    alias := exparr__get(page.aliases, i)^
    mps__labeled_str(mps, "alias", alias)
  }

  mps__block_markup(mps, page.content)
}

mps__feed :: proc(mps: ^Markup_Printer_State, feed: Def__Feed) {
  mps__deeper(mps, "feed")

  mps__labeled_str(mps, "at", feed.at)

  {mps__deeper(mps, "members"); mps__page_filter__many(mps, feed.members)}
  {mps__deeper(mps, "under"); mps__page_filter__many(mps, feed.under)}

  mps__labeled_str(mps, "name", feed.name)
  mps__labeled_str(mps, "description", feed.description)
}
// }}}

// Other ad-hoc pretty printers
// {{{ Parsing errors
@(private="package")
pretty_error :: proc(error: Error) -> string {
  builder: strings.Builder
  strings.builder_init_none(&builder, context.temp_allocator)

  switch inner in error.loc {
  case Path__Absolute: 
    fmt.sbprintf(&builder, "%v", string(inner))
  case ^File:
    fmt.sbprintf(&builder, "%v", inner.name)
  case Source_Loc:
    fmt.sbprintf(&builder, "%v(%v:%v)", inner.file.name, inner.line, inner.col)
  case Token:
    pos := inner.from
    fmt.sbprintf(&builder, "%v(%v:%v)", pos.file.name, pos.line, pos.col)
  case Source_Range:
    from := inner[0]
    to   := inner[1]   
    log.assert(from.file == to.file)

    fmt.sbprintf(
      &builder,
      "%v(%v:%v-%v:%v)",
      from.file.name,
      from.line,
      from.col,
      to.line,
      to.col,
    )
  }

  fmt.sbprint(&builder, ": ")
  fmt.sbprint(&builder, error.msg)

  return strings.to_string(builder)
}
// }}}
