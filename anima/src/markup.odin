package anima

import "base:runtime"
import "core:mem"
import "core:log"
import "core:fmt"
import "core:time"
import "core:strings"

// {{{ Page
// TODO: assets?, imports, (anima) comments
Page :: struct {
  compact:   bool, // Whether the page should not contain the post layout
  public:    bool, // Whether the page should be included in the sitemap

  filename:    Contiguous_Text, // Overrides the last segment of the path
  description: Inline_Markup,
  content:     Block_Markup,

  created_at:   time.Time, // When was the file created?
  published_at: time.Time, // When was the file created?

  // We don't bother defining sitemap-specific types since those options are
  // seldom used in practice.
  changefreq:   Contiguous_Text, // Override the change frequency for the page
  priority:     Contiguous_Text, // Assign a priority to the sitemap entry

  tags:      Exparr(Tag),
  changelog: Exparr(Change),
  headings:  Exparr(Heading),
	links:     Exparr(Def__Link),
	icons:     Exparr(Def__Icon),
  feeds:     Exparr(Def__Feed),
	footnotes: Exparr(Def__Footnote),
  aliases:   Exparr(Contiguous_Text), // Locations to redirect from
}

page__make :: proc(allocator: mem.Allocator) -> (page: Page) {
  page.links.allocator     = allocator
  page.icons.allocator     = allocator
  page.headings.allocator  = allocator
  page.footnotes.allocator = allocator
  return page
}

codec__page :: proc(k: ^Codec_Kit) -> Typed_Codec(Page) {
  ctext := codec__contiguous_text(k)
  imarkup := codec__inline_markup(k)
  bmarkup := codec__block_markup(k)
  timestamp := codec__timestamp(k)

  feeds_payload   := codec__exparr(k, codec__at(k, "feed",   codec__feed(k)))
  tags_payload    := codec__exparr(k, codec__at(k, "tag",    codec__tag(k)))
  changes_payload := codec__exparr(k, codec__at(k, "change", codec__change(k)))
  aliases_payload := codec__exparr(k, codec__at(k, "alias",  ctext))

  feeds   := codec__field(k, "feeds",     Page, feeds_payload)
  tags    := codec__field(k, "tags",      Page, tags_payload)
  changes := codec__field(k, "changelog", Page, changes_payload)
  aliases := codec__field(k, "aliases",   Page, aliases_payload)

  content     := codec__field(k, "content", Page, bmarkup, REQUIRED)
  description := codec__field_at(k, "description", Page, imarkup, UNIQUE)
  filename    := codec__field_at(k, "filename", Page, ctext, UNIQUE)

  created   := codec__field_at(k, "created_at", Page, timestamp, ONCE, ONCE)
  published := codec__field_at(k, "published_at", Page, timestamp, UNIQUE)

  priority   := codec__field_at(k, "priority",   Page, ctext, UNIQUE)
  changefreq := codec__field_at(k, "changefreq", Page, ctext, UNIQUE)

  compact := codec__flag_at(k, "compact", Page)
  public  := codec__flag_at(k, "public", Page)

  inner_loop := codec__sum(
    k, Page,
    content, feeds, tags, aliases, public, description, compact, created,
    published, filename, changefreq, priority, changes,
  )

  lens :: proc(kit: ^Lens_Kit) {
    switch kit.mode {
    case .Project:
      if mem.check_zero_ptr(kit.outer, size_of(Page)) {
        (cast(^Page)kit.outer)^ = page__make(kit.allocator)
      }

      kit.document = kit.inner
      mem.copy(kit.inner, kit.outer, size_of(Page))
    case .Inject:
      mem.copy(kit.outer, kit.inner, size_of(Page))
    }
  }

  return codec__focus(k, Page, codec__loop(k, inner_loop), lens)
}
// }}}
// {{{ Changelog entries
Change :: struct {
  at:      time.Time, 
  message: Inline_Markup,
}

codec__change :: proc(k: ^Codec_Kit) -> Typed_Codec(Change) {
  imarkup := codec__inline_markup(k)
	at := codec__field_at(k, "at", Change, codec__timestamp(k), ONCE)
  message := codec__field(k, "message", Change, imarkup, REQUIRED)

  return codec__loop(k, codec__sum(k, Change, at, message))
}
// }}}
// {{{ Page filtering
// We make this a struct, since we otherwise hit bugs in the compiler
Page_Filter__Many :: struct { elements: Exparr(Page_Filter__Atom) }

Page_Filter__Any   :: distinct Page_Filter__Many  // OR
Page_Filter__All   :: distinct Page_Filter__Many  // AND
Page_Filter__Not   :: distinct ^Page_Filter__Atom // NOT

Page_Filter__Tag   :: distinct Tag  // Pages having this tag
Page_Filter__Local :: distinct Unit // The current page
Page_Filter__Public :: distinct Unit // The page is visible

// NOTE: nil is equivalent to Page_Filter__Local
Page_Filter__Atom :: union {
  Page_Filter__All,
  Page_Filter__Any,
  Page_Filter__Not,
  Page_Filter__Tag,
  Page_Filter__Local,
  Page_Filter__Public,
}

@(private = "file")
codec__page_filter__atom :: proc(
  k: ^Codec_Kit
) -> Typed_Codec(Page_Filter__Atom) {
  return codec__memo(
  	k,
  	"page_filter__atom",
  	proc(k: ^Codec_Kit) -> Typed_Codec(Page_Filter__Atom) {
      atom := codec__page_filter__atom(k)
      many := codec__page_filter__many(k)

      local := codec__const(k, "local", Page_Filter__Local{})
      public := codec__const(k, "public", Page_Filter__Public{})
      not := codec__trans_at(k, "not", Page_Filter__Not, codec__ref(k, atom))
      all := codec__trans_at(k, "all", Page_Filter__All, many)
      any := codec__trans_at(k, "any", Page_Filter__Any, many)
      tag := codec__trans_at(k, "tag", Page_Filter__Tag, codec__tag(k))

      return codec__sum(
        k,
        Page_Filter__Atom,
        codec__variant(k, Page_Filter__Atom, local),
        codec__variant(k, Page_Filter__Atom, public),
        codec__variant(k, Page_Filter__Atom, not),
        codec__variant(k, Page_Filter__Atom, all),
        codec__variant(k, Page_Filter__Atom, any),
        codec__variant(k, Page_Filter__Atom, tag),
      )
    },
  )
}

@(private = "file")
codec__page_filter__many :: proc(
  k: ^Codec_Kit
) -> Typed_Codec(Page_Filter__Many) {
	return codec__memo(
		k,
		"page_filter__many",
		proc(k: ^Codec_Kit) -> Typed_Codec(Page_Filter__Many) {
      inner := codec__spaced_exparr(k, codec__page_filter__atom(k))
			return codec__transmute(k, Page_Filter__Many, inner)
		},
	)
}

@(private = "file")
codec__page_filter__all :: proc(
  k: ^Codec_Kit
) -> Typed_Codec(Page_Filter__All) {
	return codec__memo(
		k,
		"page_filter__all",
		proc(k: ^Codec_Kit) -> Typed_Codec(Page_Filter__All) {
			return codec__transmute(k, Page_Filter__All, codec__page_filter__many(k))
		},
	)
}
// }}}
// {{{ Icon definitions
Def__Icon :: struct {
  id:    Contiguous_Text,
  path:  Contiguous_Text,
  scope: Page_Filter__All,
}

@(private = "file")
codec__deficon :: proc(k: ^Codec_Kit) -> Typed_Codec(^Def__Icon) {
  Self :: Def__Icon

  ctext := codec__contiguous_text(k)
  filter := codec__page_filter__all(k)

	id := codec__field(k, "id", Self, ctext, REQUIRED)
	path := codec__field_at(k, "path", Self, ctext, ONCE)
	scope := codec__field_at(k, "scope", Self, filter, ONCE)

  inner_loop := codec__loop(k, codec__sum(k, Self, id, path, scope))
	return codec__remote_push(k, "icons", inner_loop)
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
codec__deflink :: proc(k: ^Codec_Kit) -> Typed_Codec(^Def__Link) {
  ctext := codec__contiguous_text(k)
  imarkup := codec__inline_markup(k)

	id := codec__field_at(k, "id", Def__Link, ctext, ONCE)
  target := codec__field_at(k, "target", Def__Link, ctext, ONCE)
  label := codec__field(k, "label", Def__Link, imarkup, REQUIRED)
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
codec__defnote :: proc(k: ^Codec_Kit) -> Typed_Codec(^Def__Footnote) {
  ctext := codec__contiguous_text(k)
  bmarkup := codec__block_markup(k)

	id := codec__field_at(k, "id", Def__Footnote, ctext, ONCE)
  content := codec__field(k, "content", Def__Footnote, bmarkup, REQUIRED)
  inner_loop := codec__loop(k, codec__sum(k, Def__Footnote, content, id))
	return codec__remote_push(k, "footnotes", inner_loop)
}
// }}}
// {{{ Feed definitions
Def__Feed :: struct {
  id:          Contiguous_Text,
  description: Inline_Markup,

  members: Page_Filter__All, // What posts should this include?
  under:   Page_Filter__All, // Which pages should this appear on?
}

@(private = "file")
codec__feed :: proc(k: ^Codec_Kit) -> Typed_Codec(Def__Feed) {
  Self :: Def__Feed

  ctext := codec__contiguous_text(k)
  imarkup := codec__inline_markup(k)
  filter := codec__page_filter__all(k)

	id := codec__field_at(k, "id", Self, ctext)
	desc := codec__field(k, "description", Self, imarkup, REQUIRED)
	under := codec__field_at(k, "under", Self, filter, ONCE)
	members := codec__field_at(k, "members", Self, filter, ONCE)

  return codec__loop(k, codec__sum(k, Self, id, under, members, desc))
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
  imarkup := codec__inline_markup(k)

	id := codec__field_at(k, "id", Heading, ctext, UNIQUE)
  content := codec__field(k, "content", Heading, imarkup, REQUIRED)
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
  imarkup := codec__inline_markup(k)
	cell_payload := codec__field(k, "content", Table__Cell, imarkup)
	cell := codec__at(k, "cell", cell_payload)
	row := codec__field(k, "cells", Table__Row, codec__spaced_exparr(k, cell))

	caption := codec__field(k, "caption", Table, imarkup)
	header := codec__field_at(k, "header", Table, row, ONCE)

  rows_payload := codec__exparr(k, codec__at(k, "row", row))
	rows := codec__field(k, "rows", Table, rows_payload)

	return codec__loop(k, codec__sum(k, Table, caption, header, rows))
}
// }}}
// {{{ Inline_Markup__Timestamp
@(private = "file")
codec__timestamp :: proc(k: ^Codec_Kit) -> Typed_Codec(time.Time) {
  lens :: proc(kit: ^Lens_Kit) {
    outer := cast(^time.Time)kit.outer
    inner := cast(^Contiguous_Text)kit.inner
    switch kit.mode {
    case .Project:
      log.assertf(outer^ == {}, "Timestamps must parse in one go %v", outer^)
    case .Inject:
      as_string := contiguous_text__concat(inner^, kit.temp_allocator)
      if as_string == "" {
        kit.ignore_consumption = true
        return
      }

      datetime, datetime_consumed := time.iso8601_to_time_utc(as_string)

      if datetime_consumed > 0 {
        outer^ = datetime
        return
      }

      // Try to tack an empty timestamp at the end
      as_date_string := fmt.aprintf(
        "%vT00:00:00+00:00",
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

  return codec__tracked(
    k,
    codec__focus(k, time.Time, codec__contiguous_text(k), lens),
    "timestamp",
    UNIQUE
  )
}
// }}}
// {{{ Contiguous text
// A sequence of text where all the whitespace in the source is discarded
Contiguous_Text :: distinct Exparr(string)

@(private = "file")
codec__contiguous_text :: proc(kit: ^Codec_Kit) -> Typed_Codec(Contiguous_Text) {
	return codec__memo(
		kit,
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
// {{{ Tag
Tag :: distinct Contiguous_Text

@(private = "file")
codec__tag :: proc(k: ^Codec_Kit) -> Typed_Codec(Tag) {
  return codec__transmute(k, Tag, codec__contiguous_text(k))
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

Inline_Markup__Timestamp :: struct {
  compact: bool, // Shortens the output
  time: time.Time,
}

Inline_Markup__Date :: distinct Inline_Markup__Timestamp
Inline_Markup__Datetime :: distinct Inline_Markup__Timestamp

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
codec__inline_markup__timestamp :: proc(
  k: ^Codec_Kit
) -> Typed_Codec(Inline_Markup__Timestamp) {
  Self :: Inline_Markup__Timestamp

  time := codec__field(k, "time", Self, codec__timestamp(k), ONCE)
  compact := codec__flag_at(k, "compact", Self)

  return codec__loop(k, codec__sum(k, Self, time, compact))
}

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
  link_id := codec__field(k, "id", Link, ctext, ONCE)
  link_label := codec__field_at(k, "label", Link, imarkup, UNIQUE)
  link_sum := codec__sum(k, Link, link_label, link_id)
  link := codec__at(k, "link", codec__loop(k, link_sum))

  timestamp := codec__inline_markup__timestamp(k)
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
		"inline_markup",
		proc(k: ^Codec_Kit) -> Typed_Codec(Inline_Markup) {
      lens :: proc(kit: ^Lens_Kit) {
        switch kit.mode {
        case .Project:
          mem.copy(kit.inner, kit.outer, size_of(Inline_Markup))
        case .Inject:
          inner := cast(^Exparr(Inline_Markup__Atom))kit.inner

          found_substantial := false
          for i in 0..<inner.len {
            elem := exparr__get(inner^, i)
            if _, ok := elem.(Inline_Markup__Space); !ok {
              found_substantial = true
              break
            }
          }

          if found_substantial {
            mem.copy(kit.outer, kit.inner, size_of(Inline_Markup))
          } else {
            kit.ignore_consumption = true
          }
        }
      }

			return codec__focus(
				k,
				Inline_Markup,
				codec__exparr(k, codec__inline_markup__atom(k)),
        lens
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

Block_Markup__Aside :: struct {
  id:       Contiguous_Text,
  char:     Contiguous_Text,
  content:  Block_Markup,
  title:    Inline_Markup,

  // Whether to hide the content by default
  collapse: bool,
}

Block_Markup__Code :: struct {
  language: Contiguous_Text,
  content:  Contiguous_Text,
}

Block_Markup__Blockquote :: distinct Block_Markup
Block_Markup__Description :: distinct Unit
Block_Markup__Table_Of_Contents :: distinct Unit
Block_Markup__Thematic_Break :: distinct Unit
Block_Markup__Index :: distinct Page_Filter__All

// TODO: code, list
Block_Markup__Atom :: union {
	Block_Markup__Paragraph,
	Block_Markup__Image,
	Block_Markup__Figure,
	Block_Markup__List, // todo
	Block_Markup__Blockquote,
	Block_Markup__Description,
	Block_Markup__Table_Of_Contents,
	Block_Markup__Thematic_Break,
  Block_Markup__Index,
  Block_Markup__Aside,
	Table,

  // References to data saved in the parent Page structure
  ^Def__Link,
  ^Def__Footnote,
  ^Def__Icon,
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
  ctext := codec__contiguous_text(k)
	imarkup := codec__inline_markup(k)
	source_ref := codec__field_at(k, "source", Block_Markup__Image, ctext, ONCE)
	alt_ref := codec__field(k, "alt", Block_Markup__Image, imarkup, ONCE)
	return codec__loop(k, codec__sum(k, Block_Markup__Image, source_ref, alt_ref))
}

@(private = "file")
codec__block_markup__figure :: proc(
  k: ^Codec_Kit
) -> Typed_Codec(Block_Markup__Figure) {
  imarkup := codec__inline_markup(k)
  bmarkup := codec__block_markup(k)
	caption := codec__field_at(k, "caption", Block_Markup__Figure, imarkup, UNIQUE)
	content := codec__field(k, "content", Block_Markup__Figure, bmarkup, ONCE)
	return codec__loop(k, codec__sum(k, Block_Markup__Figure, caption, content))
}

@(private = "file")
codec__block_markup__aside :: proc(
  k: ^Codec_Kit
) -> Typed_Codec(Block_Markup__Aside) {
  Self :: Block_Markup__Aside

  ctext   := codec__contiguous_text(k)
  imarkup := codec__inline_markup(k)
  bmarkup := codec__block_markup(k)

  id       := codec__field_at(k, "id", Self, ctext, UNIQUE)
  icon     := codec__field_at(k, "char", Self, ctext, UNIQUE)
  title    := codec__field_at(k, "title", Self, imarkup, UNIQUE)
	content  := codec__field(k, "content", Self, bmarkup, ONCE)
  collapse := codec__flag_at(k, "collapse", Self)

	return codec__loop(
    k, 
    codec__sum(k, Self, content, id, icon, title, collapse, content)
  )
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
	deflink := codec__at(k, "deflink", codec__deflink(k))
	defnote := codec__at(k, "defnote", codec__defnote(k))
  deficon := codec__at(k, "deficon", codec__deficon(k))
	aside := codec__at(k, "aside", codec__block_markup__aside(k))

	h1 := codec__at(k, "#", codec__heading(k, 1))
	h2 := codec__at(k, "#", codec__heading(k, 2))
	h3 := codec__at(k, "#", codec__heading(k, 3))

  filter__all := codec__page_filter__all(k)
	index := codec__trans_at(k, "index", Block_Markup__Index, filter__all)

	return codec__sum(
		k,
		Block_Markup__Atom,
		codec__variant(k, Block_Markup__Atom, blockquote),
		codec__variant(k, Block_Markup__Atom, description),
		codec__variant(k, Block_Markup__Atom, table_of_contents),
		codec__variant(k, Block_Markup__Atom, thematic_break),
		codec__variant(k, Block_Markup__Atom, image),
		codec__variant(k, Block_Markup__Atom, figure),
		codec__variant(k, Block_Markup__Atom, table),
		codec__variant(k, Block_Markup__Atom, h1),
		codec__variant(k, Block_Markup__Atom, h2),
		codec__variant(k, Block_Markup__Atom, h3),
		codec__variant(k, Block_Markup__Atom, index),
		codec__variant(k, Block_Markup__Atom, aside),
		codec__variant(k, Block_Markup__Atom, deflink),
		codec__variant(k, Block_Markup__Atom, defnote),
		codec__variant(k, Block_Markup__Atom, deficon),
		codec__variant(k, Block_Markup__Atom, para),
	)
}

codec__block_markup :: proc(kit: ^Codec_Kit) -> Typed_Codec(Block_Markup) {
	return codec__memo(
		kit,
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
