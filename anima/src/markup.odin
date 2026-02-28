package anima

import "base:runtime"
import "core:mem"
import "core:fmt"
import "core:time"
import "core:strings"

// {{{ Page
// TODO: description, filename, aliases, created_at, changelog, feeds, tags, 
// assets, imports, (anima) comments
Page :: struct {
  compact:   bool, // Whether the page should not contain the post layout
  public:    bool, // Whether the page should be included in the sitemap
  published: bool, // Whether the article should not be a draft

  description:  Block_Markup,
  created_at:   Timestamp,       // When was the file created?
  published_at: Timestamp,       // When was the file created?
  filename:     Contiguous_Text, // Overrides the last segment of the path
  changefreq:   Contiguous_Text, // Override the change frequency for the page
  priority:     f32,             // Assign a priority to the sitemap entry

  tags:      Exparr(Tag),
  changelog: Exparr(Change),
  headings:  Exparr(Heading),
	links:     Exparr(Def__Link),
	icons:     Exparr(Def__Icon),
  feeds:     Exparr(Def__Feed),
	footnotes: Exparr(Def__Footnote),
  aliases:   Exparr(Contiguous_Text), // Locations to redirect from
}

Tag :: distinct Contiguous_Text

Change :: struct {
  at:      Timestamp, 
  Message: Inline_Markup,
}

page__make :: proc(allocator: mem.Allocator) -> (page: Page) {
  page.links.allocator     = allocator
  page.headings.allocator  = allocator
  page.footnotes.allocator = allocator
  return page
}
// }}}
// {{{ Page filtering
Page_Filter__Any        :: distinct Exparr(Page_Filter) // OR
Page_Filter__All        :: distinct Exparr(Page_Filter) // AND
Page_Filter__Not        :: distinct ^Page_Filter        // NOT
Page_Filter__Tag        :: distinct Tag                 // Pages having this tag
Page_Filter__Local      :: distinct Unit                // The current page
Page_Filter__Everything :: distinct Unit                // Every page

Page_Filter :: union {
  Page_Filter__All,
  Page_Filter__Any,
  Page_Filter__Not,
  Page_Filter__Tag,
}
// }}}
// {{{ Icon definitions
Def__Icon :: struct {
  id:    string,
  path:  Contiguous_Text,
  scope: Page_Filter__All,
}
// }}}
// {{{ Link definitions
Def__Link :: struct {
	id:     Contiguous_Text,
	target: Contiguous_Text, // url
	label:  Inline_Markup,
  scope:  Page_Filter__All, // Link definitions can affect other pages
}

@(private = "file")
codec__linkdef :: proc(k: ^Codec_Kit) -> Typed_Codec(^Def__Link) {
  ctext := codec__contiguous_text(k)
	id := codec__field_at(k, "id", Def__Link, codec__once(k, ctext))
  target := codec__field_at(k, "target", Def__Link, codec__once(k, ctext))
  label := codec__field(k, "label", Def__Link, codec__inline_markup(k))
  inner_loop := codec__loop(k, codec__sum(k, Def__Link, label, target, id))
	return codec__remote_push(k, "links", inner_loop)
}
// }}}
// {{{ Footnote definitions
Def__Footnote :: struct {
	id:      Contiguous_Text,
	content: Block_Markup,
}

@(private = "file")
codec__fndef :: proc(k: ^Codec_Kit) -> Typed_Codec(^Def__Footnote) {
  ctext := codec__contiguous_text(k)
	id := codec__field_at(k, "id", Def__Footnote, codec__once(k, ctext))
  content := codec__field(k, "content", Def__Footnote, codec__block_markup(k))
  inner_loop := codec__loop(k, codec__sum(k, Def__Footnote, content, id))
	return codec__remote_push(k, "footnotes", inner_loop)
}
// }}}
// {{{ Feed definitions
Def__Feed :: struct {
  id:           string, // TODO: perhaps make the whole path configurable?
  description:  Inline_Markup,
  advertise_on: Page_Filter__All, // Which pages should this appear on?
  elements:     Page_Filter__All, // What posts should this include?
}
// }}}
// {{{ Heading
Heading :: struct {
  id:      Contiguous_Text,
  content: Inline_Markup,
  level:   uint,
}

@(private = "file")
codec__heading :: proc(k: ^Codec_Kit, level: uint) -> Typed_Codec(^Heading) {
  // This would benefit from having a "tap"-style helper, but oh well.
  lens :: proc(kit: ^Lens_Kit) {
    outer := cast(^Heading)kit.outer
    inner := cast(^Heading)kit.inner

    switch kit.mode {
    case .Project: inner^ = outer^
    case .Inject:
      outer.level = uint(uintptr(kit.user_data))
      outer^ = inner^
    }
  }

  ctext := codec__contiguous_text(k)
	id := codec__field_at(k, "id", Heading, codec__once(k, ctext))
  content := codec__field(k, "content", Heading, codec__inline_markup(k))
  looped := codec__loop(k, codec__sum(k, Heading, content, id))
  with_level := codec__focus(k, Heading, looped, lens, rawptr(uintptr(level)))
	return codec__remote_push(k, "headings", with_level)
}
// }}}
// {{{ Table
Table__Cell :: struct {
	content: Inline_Markup,
}

Table__Row :: struct {
	cells: Exparr(Table__Cell),
}

Table :: struct {
	caption: Inline_Markup,
	header:  Table__Row,
	rows:    Exparr(Table__Row),
}

@(private = "file")
codec__table :: proc(k: ^Codec_Kit) -> Typed_Codec(Table) {
	cell_payload := codec__field(k, "content", Table__Cell, codec__inline_markup(k))
	cell := codec__at(k, "cell", cell_payload)
	row := codec__field(k, "cells", Table__Row, codec__spaced_exparr(k, cell))

	caption := codec__field(k, "caption", Table, codec__inline_markup(k))
	header := codec__field_at(k, "header", Table, codec__once(k, row))
	rows := codec__field(k, "rows", Table, codec__exparr(k, codec__at(k, "row", row)))

	return codec__loop(k, codec__sum(k, Table, caption, header, rows))
}
// }}}
// {{{ Timestamp
Timestamp :: struct {
  compact: bool, // Shortens the output
  time:    time.Time
}

codec__timestamp :: proc(k: ^Codec_Kit) -> Typed_Codec(Timestamp) {
  lens :: proc(kit: ^Lens_Kit) {
    outer := cast(^time.Time)kit.outer
    inner := cast(^Contiguous_Text)kit.inner
    if kit.mode == .Inject {
      as_string := contiguous_text__concat(inner^, kit.temp_allocator)
      datetime, datetime_consumed := time.iso8601_to_time_utc(as_string)

      if datetime_consumed > 0 {
        outer^ = datetime
        return
      }

			// Try to tack an empty timestamp at the end
			as_date_string := fmt.aprintf("%vT00:00:00+00:00",
				as_string,
				allocator = kit.temp_allocator,
			)

			date, date_consumed := time.iso8601_to_time_utc(as_date_string)

      if date_consumed > 0 {
        outer^ = date
        return
      }

      lens__errorf(kit, "Invalid timestamp: '%v'", as_string)
    }
  }

  codec :: proc(k: ^Codec_Kit) -> Typed_Codec(Timestamp) {
    time_payload := codec__focus(
      k,
      time.Time,
      codec__contiguous_text(k),
      lens
    )

    time := codec__field(k, "time", Timestamp, codec__once(k, time_payload))
    compact := codec__flag_at(k, "compact", Timestamp)

    return codec__loop(k, codec__sum(k, Timestamp, time, compact))
  }

  return codec__memo(k, Timestamp, "timestamp", codec)
}
// }}}
// {{{ Contiguous text
// A sequence of text where all the whitespace in the source is discarded
Contiguous_Text :: distinct Exparr(string)
codec__contiguous_text :: proc(kit: ^Codec_Kit) -> Typed_Codec(Contiguous_Text) {
	return codec__memo(
		kit,
		Contiguous_Text,
		"contiguous_text",
		proc(kit: ^Codec_Kit) -> Typed_Codec(Contiguous_Text) {
      // TODO: disallow empty strings
			return codec__transmute(
				kit,
				Contiguous_Text,
				codec__spaced_exparr(kit, codec__string(kit)),
			)
		},
	)
}

contiguous_text__concat :: proc(
  ctext: Contiguous_Text, allocator: mem.Allocator
) -> string {
  size := 0
  exparr := cast(Exparr(string))ctext
  for i in 0..<exparr.len do size += len(exparr__get(exparr, i))

  // Allocate a string buffer, preventing further re-allocations
  builder := strings.builder_make_len_cap(0, size, allocator)
  builder.buf.allocator = runtime.panic_allocator()

  for i in 0..<exparr.len {
    strings.write_string(&builder, exparr__get(exparr, i)^)
  }

  return strings.to_string(builder)
}
// }}}

// {{{ Inline
Inline_Markup__Space :: distinct Unit
Inline_Markup__Ellipsis :: distinct Unit
Inline_Markup__Text :: distinct string
Inline_Markup__Emph :: distinct Inline_Markup
Inline_Markup__Strong :: distinct Inline_Markup
Inline_Markup__Strikethrough :: distinct Inline_Markup
Inline_Markup__Mono :: distinct Inline_Markup
Inline_Markup__Quote :: distinct Inline_Markup
Inline_Markup__Icon :: distinct Contiguous_Text
Inline_Markup__Fn :: distinct Contiguous_Text
Inline_Markup__Link :: struct {
	id:    Contiguous_Text,
	label: Inline_Markup,
}

Inline_Markup__Date :: distinct Timestamp
Inline_Markup__Datetime :: distinct Timestamp

// Using distinct runs into circular types issue (for no reason)
Inline_Markup :: struct {
	elements: Exparr(Inline_Markup__Atom),
}

// TODO: LaTeX
Inline_Markup__Atom :: union {
	Inline_Markup__Space,
	Inline_Markup__Ellipsis,
	Inline_Markup__Text,
	Inline_Markup__Emph,
	Inline_Markup__Strong,
	Inline_Markup__Strikethrough,
	Inline_Markup__Mono,
	Inline_Markup__Quote,
	Inline_Markup__Icon,
	Inline_Markup__Fn,
	Inline_Markup__Link,
	Inline_Markup__Date,
	Inline_Markup__Datetime,
}
// }}}
// {{{ Inline Codecs
@(private = "file")
codec__inline_markup__atom :: proc(
  k: ^Codec_Kit
) -> Typed_Codec(Inline_Markup__Atom) {
	imarkup := codec__inline_markup(k)
	ctext := codec__contiguous_text(k)

	space := codec__space(k, Inline_Markup__Space)
	text := codec__transmute(k, Inline_Markup__Text, codec__string(k))
	ellipsis := codec__const(k, "...", Inline_Markup__Ellipsis{})
	emph := codec__trans_at(k, "_", Inline_Markup__Emph, imarkup)
	strong := codec__trans_at(k, "*", Inline_Markup__Strong, imarkup)
	strike := codec__trans_at(k, "~", Inline_Markup__Strikethrough, imarkup)
	mono := codec__trans_at(k, "`", Inline_Markup__Mono, imarkup)
	quote := codec__trans_at(k, "\"", Inline_Markup__Quote, imarkup)
  icon := codec__trans_at(k, "icon", Inline_Markup__Icon, ctext)
  fn := codec__trans_at(k, "fn", Inline_Markup__Fn, ctext)

  Link :: Inline_Markup__Link
  link_id := codec__field(k, "id", Link, ctext)
  link_label := codec__field_at(k, "label", Link, codec__once(k, imarkup))
  link_sum := codec__sum(k, Link, link_label, link_id)
  link := codec__at(k, "link", codec__loop(k, link_sum))

  timestamp := codec__timestamp(k)
  date := codec__trans_at(k, "date", Inline_Markup__Date, timestamp)
  datetime := codec__trans_at(k, "datetime", Inline_Markup__Datetime, timestamp)

	return codec__sum(
		k,
		Inline_Markup__Atom,
		codec__variant(k, Inline_Markup__Atom, space),
		codec__variant(k, Inline_Markup__Atom, text),
		codec__variant(k, Inline_Markup__Atom, ellipsis),
		codec__variant(k, Inline_Markup__Atom, emph),
		codec__variant(k, Inline_Markup__Atom, strong),
		codec__variant(k, Inline_Markup__Atom, strike),
		codec__variant(k, Inline_Markup__Atom, mono),
		codec__variant(k, Inline_Markup__Atom, quote),
		codec__variant(k, Inline_Markup__Atom, icon),
		codec__variant(k, Inline_Markup__Atom, fn),
		codec__variant(k, Inline_Markup__Atom, link),
		codec__variant(k, Inline_Markup__Atom, date),
		codec__variant(k, Inline_Markup__Atom, datetime),
	)
}

codec__inline_markup :: proc(kit: ^Codec_Kit) -> Typed_Codec(Inline_Markup) {
	return codec__memo(
		kit,
		Inline_Markup,
		"inline_markup",
		proc(kit: ^Codec_Kit) -> Typed_Codec(Inline_Markup) {
			return codec__transmute(
				kit,
				Inline_Markup,
				codec__exparr(kit, codec__inline_markup__atom(kit)),
			)
		},
	)
}
// }}}

// {{{ Block
Block_Markup__Paragraph :: distinct Inline_Markup

Block_Markup__Image :: struct {
	alt:    Inline_Markup,
	source: Contiguous_Text,
}

Block_Markup__Figure :: struct {
	caption: Inline_Markup,
	content: Block_Markup,
}

Block_Markup__List :: struct {
	ordered:  bool,
	block:    bool,
	elements: union #no_nil {
		Exparr(Inline_Markup),
		Exparr(Block_Markup),
	},
}

Block_Markup__Blockquote :: distinct Block_Markup
Block_Markup__Description :: distinct Unit
Block_Markup__Table_Of_Contents :: distinct Unit
Block_Markup__Thematic_Break :: distinct Unit

// TODO: code, aside, index
Block_Markup__Atom :: union {
	Block_Markup__Paragraph,
	Block_Markup__Image,
	Block_Markup__Figure,
	Block_Markup__List, // todo
	Block_Markup__Blockquote,
	Block_Markup__Description,
	Block_Markup__Table_Of_Contents,
	Block_Markup__Thematic_Break,
	Table,

  // References to data saved in the parent Page structure
  ^Def__Link,
  ^Def__Footnote,
  ^Heading,
}

Block_Markup :: struct {
	elements: Exparr(Block_Markup__Atom),
}
// }}}
// {{{ Block Codecs
@(private = "file")
codec__block_markup__image :: proc(
  k: ^Codec_Kit
) -> Typed_Codec(Block_Markup__Image) {
	source := codec__at(k, "source", codec__once(k, codec__contiguous_text(k)))
	source_ref := codec__field(k, "source", Block_Markup__Image, source)
	alt := codec__inline_markup(k)
	alt_ref := codec__field(k, "alt", Block_Markup__Image, alt)
	return codec__loop(k, codec__sum(k, Block_Markup__Image, source_ref, alt_ref))
}

@(private = "file")
codec__block_markup__figure :: proc(
  k: ^Codec_Kit
) -> Typed_Codec(Block_Markup__Figure) {
  imarkup := codec__inline_markup(k)
  bmarkup := codec__block_markup(k)
	caption := codec__at(k, "caption", codec__once(k, imarkup))
	caption_ref := codec__field(k, "caption", Block_Markup__Figure, caption)
	content := codec__field(k, "content", Block_Markup__Figure, bmarkup)
	return codec__loop(k, codec__sum(k, Block_Markup__Figure, caption_ref, content))
}

@(private = "file")
codec__block_markup__atom :: proc(
  k: ^Codec_Kit
) -> Typed_Codec(Block_Markup__Atom) {
	imarkup := codec__inline_markup(k)
	bmarkup := codec__block_markup(k)
	description := codec__const(k, "embed-description", Block_Markup__Description{})
	thematic_break := codec__const(k, "---", Block_Markup__Thematic_Break{})
	table_of_contents := codec__const(k, "toc", Block_Markup__Table_Of_Contents{})
	blockquote := codec__trans_at(k, ">", Block_Markup__Blockquote, bmarkup)
	image := codec__at(k, "image", codec__block_markup__image(k))
	figure := codec__at(k, "figure", codec__block_markup__figure(k))
	para := codec__transmute(k, Block_Markup__Paragraph, codec__para(k, imarkup))
	table := codec__at(k, "table", codec__table(k))
	linkdef := codec__at(k, "linkdef", codec__linkdef(k))
	fndef := codec__at(k, "fndef", codec__fndef(k))
	h1 := codec__at(k, "#", codec__heading(k, 1))
	h2 := codec__at(k, "#", codec__heading(k, 2))
	h3 := codec__at(k, "#", codec__heading(k, 3))
	// Block_Markup__List,

	return codec__sum(
		k,
		Block_Markup__Atom,
		codec__variant(k, Block_Markup__Atom, blockquote),
		codec__variant(k, Block_Markup__Atom, description),
		codec__variant(k, Block_Markup__Atom, table_of_contents),
		codec__variant(k, Block_Markup__Atom, thematic_break),
		codec__variant(k, Block_Markup__Atom, image),
		codec__variant(k, Block_Markup__Atom, figure),
		codec__variant(k, Block_Markup__Atom, para),
		codec__variant(k, Block_Markup__Atom, table),
		codec__variant(k, Block_Markup__Atom, linkdef),
		codec__variant(k, Block_Markup__Atom, fndef),
		codec__variant(k, Block_Markup__Atom, h1),
		codec__variant(k, Block_Markup__Atom, h2),
		codec__variant(k, Block_Markup__Atom, h3),
	)
}

codec__block_markup :: proc(kit: ^Codec_Kit) -> Typed_Codec(Block_Markup) {
	return codec__memo(
		kit,
		Block_Markup,
		"block_markup",
		proc(kit: ^Codec_Kit) -> Typed_Codec(Block_Markup) {
			return codec__transmute(
				kit,
				Block_Markup,
				codec__spaced_exparr(kit, codec__block_markup__atom(kit)),
			)
		},
	)
}
// }}}
