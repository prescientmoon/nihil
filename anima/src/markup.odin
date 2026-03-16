package anima

import "base:runtime"
import "core:mem"
import "core:mem/virtual"
import "core:log"
import "core:fmt"
import "core:time"
import "core:strings"
import "core:container/small_array"

// {{{ Page
// TODO: assets?, imports, (anima) comments
Page :: struct {
  compact:   bool, // Whether the page should not contain the post layout
  public:    bool, // Whether the page should be included in the sitemap

  filename:    string, // Overrides the last segment of the path
  title:       Inline_Markup,
  description: Inline_Markup,
  content:     Block_Markup,

  created_at:   time.Time, // When was the file created?
  published_at: time.Time, // When was the file created?

  // We don't bother defining sitemap-specific types since those options are
  // seldom used in practice.
  changefreq:   string, // Override the change frequency for the page
  priority:     string, // Assign a priority to the sitemap entry

  tags:      Exparr(Tag),
  changelog: Exparr(Change),
  headings:  Exparr(Heading), // The first heading is declared the title
	links:     Exparr(Def__Link),
	icons:     Exparr(Def__Icon),
  feeds:     Exparr(Def__Feed),
	footnotes: Exparr(Def__Footnote),
  aliases:   Exparr(string), // Locations to redirect from

  // These attributes are not part of the markup itself
  source_path: Path__Absolute,
  site_path:   Path__Relative,
}

page__make :: proc(allocator: mem.Allocator) -> (page: Page) {
  page.links.allocator     = allocator
  page.icons.allocator     = allocator
  page.headings.allocator  = allocator
  page.footnotes.allocator = allocator
  return page
}

page__last_updated :: proc(page: Page) -> (t: time.Time) {
  t = time__max(page.created_at, page.published_at)
  for i in 0..<page.changelog.len {
    change := exparr__get(page.changelog, i)
    t = time__max(t, change.at)
  }

  return t
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
  title       := codec__field_at(k, "title", Page, imarkup, ONCE)
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
    content, feeds, tags, aliases, public, title, description, compact, created,
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
// {{{ Checking
page__check :: proc(site: ^Site, page: ^Page) {
  inline_markup__check(site, page^, &page.description)
  block_markup__check(site, page^, &page.content)

  // TODO: chech for duplicate tags
  // TODO: check for duplicate element IDs across  pages
  // TODO: check for duplicate footnotes
  // TODO: move link resolution checking here
  // TODO: ensure there's at most one table of contens

  for i in 0..<page.footnotes.len {
    footnote := exparr__get(page.footnotes, i)
    block_markup__check(site, page^, &footnote.content)
  }

  for i in 0..<page.links.len {
    link := exparr__get(page.links, i)
    inline_markup__check(site, page^, &link.label)
  }

  level: uint = 1 // the title is equivalent to a h1
  for i in 0..<page.headings.len {
    heading := exparr__get(page.headings, i)
    inline_markup__check(site, page^, &heading.content)

    if heading.level > level + 1 {
      site__errorf(
        site,
        heading.loc,
        "Heading increases level by more than 1"
      )
    }

    // TODO: generate ID
    // 1. genertate text
    // 2. replace " "  with "-"
    // 3. make eveything lowercase
    // 4. only keep alphanumeric characters

    level = heading.level
  }
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

page_filter__eval :: proc(base, page: Page, filter: Page_Filter__Atom) -> bool {
  switch inner in filter {
  case Page_Filter__Not: 
    if inner == nil do return false
    return !page_filter__eval(base, page, inner^)
  case Page_Filter__Public: return page.public
  case Page_Filter__All: return page_filter__all__eval(base, page, inner)
  case Page_Filter__Local: return page.site_path == base.site_path
  case Page_Filter__Any: 
    for i in 0..<inner.elements.len {
      filter := exparr__get(inner.elements, i)^
      if page_filter__eval(base, page, filter) do return true
    }

    return false
  case Page_Filter__Tag: 
    goal := Tag(inner)
    for i in 0..<page.tags.len {
      tag := exparr__get(page.tags, i)^
      if goal == tag do return true
    }

    return false
  }

  log.panic("impossible")
}

page_filter__all__eval :: proc(base, page: Page, all: Page_Filter__All) -> bool {
  if all.elements.len == 0 {
    return page_filter__eval(base, page, Page_Filter__Local{})
  }

  for i in 0..<all.elements.len {
    filter := exparr__get(all.elements, i)^
    if !page_filter__eval(base, page, filter) do return false
  }

  return true
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
  id:    string,
  path:  string,
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
	return codec__remote_push(k, "icons", Page, inner_loop)
}
// }}}
// {{{ Link definitions
Def__Link :: struct {
	id:     string,
	target: string, // url
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
	return codec__remote_push(k, "links", Page, inner_loop)
}
// }}}
// {{{ Footnote definitions
Def__Footnote :: struct {
	id:      string,
	content: Block_Markup,
  index:   uint, // The page-local number used to display the footnote
}

@(private = "file")
codec__defnote :: proc(k: ^Codec_Kit) -> Typed_Codec(^Def__Footnote) {
  ctext := codec__contiguous_text(k)
  bmarkup := codec__block_markup(k)

	id := codec__field_at(k, "id", Def__Footnote, ctext, ONCE)
  content := codec__field(k, "content", Def__Footnote, bmarkup, REQUIRED)
  inner_loop := codec__loop(k, codec__sum(k, Def__Footnote, content, id))
	return codec__remote_push(k, "footnotes", Page, inner_loop)
}
// }}}
// {{{ Feed definitions
Def__Feed :: struct {
  // NOTE: this path is currently always assumed to be relative to the root of
  // the website. I might or might not support paths relative to the current
  // page (with absolute paths being considered relative to the website root) in
  // the future (if I feel the need for something like that).
  at:          string,
  name:        string,
  description: string,

  members: Page_Filter__All, // What posts should this include?
  under:   Page_Filter__All, // Which pages should this appear on?
}

@(private = "file")
codec__feed :: proc(k: ^Codec_Kit) -> Typed_Codec(Def__Feed) {
  Self :: Def__Feed

  text := codec__text(k)
  ctext := codec__contiguous_text(k)
  filter := codec__page_filter__all(k)

	at := codec__field(k, "at", Self, ctext, REQUIRED)
	name := codec__field_at(k, "name", Self, text, REQUIRED)
	desc := codec__field_at(k, "description", Self, text, REQUIRED)
	under := codec__field_at(k, "under", Self, filter, ONCE, UNIQUE)
	members := codec__field_at(k, "members", Self, filter, ONCE)

  return codec__loop(k, codec__sum(k, Self, at, under, members, name, desc))
}
// }}}
// {{{ Headings
MAX_HEADING_LEVEL :: 4
Heading :: struct {
  id:      string,
  content: Inline_Markup,
  level:   uint,
  loc:     Source_Loc,
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
      outer^ = inner^
      outer.level = uint(uintptr(kit.user_data))
      log.assert(0 < outer.level)
      log.assert(outer.level <= MAX_HEADING_LEVEL)
    }
  }

  ctext := codec__contiguous_text(k)
  imarkup := codec__inline_markup(k)

	id := codec__field_at(k, "id", Heading, ctext, UNIQUE)
  content := codec__field(k, "content", Heading, imarkup, REQUIRED)
  looped := codec__loop(k, codec__sum(k, Heading, content, id))
  with_level := codec__focus(k, Heading, looped, lens, rawptr(uintptr(level)))
	return codec__remote_push(k, "headings", Page, codec__loc(k, with_level))
}
// }}}
// {{{ Tables
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

table__check :: proc(site: ^Site, page: Page, table: ^Table) {
  inline_markup__check(site, page, &table.caption)
  table__row__check(site, page, &table.header)
  for i in 0..<table.rows.len {
    table__row__check(site, page, exparr__get(table.rows, i))
  }
}

table__row__check :: proc(site: ^Site, page: Page, row: ^Table__Row) {
  for i in 0..<row.cells.len {
    cell := exparr__get(row.cells, i)
    mem__non_zero(cell.content.elements) or_continue
    inline_markup__check(site, page, &cell.content)
  }
}
// }}}
// {{{ Timestamps
@(private = "file")
codec__timestamp :: proc(k: ^Codec_Kit) -> Typed_Codec(time.Time) {
  lens :: proc(kit: ^Lens_Kit) {
    outer := cast(^time.Time)kit.outer
    inner := cast(^string)kit.inner
    switch kit.mode {
    case .Project:
      log.assertf(outer^ == {}, "Timestamps must parse in one go %v", outer^)
    case .Inject:
      if inner^ == "" {
        kit.consumed = false
        return
      }

      datetime, datetime_consumed := time.iso8601_to_time_utc(inner^)

      if datetime_consumed > 0 {
        outer^ = datetime
        return
      }

      // Try to tack an empty timestamp at the end
      as_date_string := fmt.aprintf(
        "%vT00:00:00+00:00",
        inner^,
        allocator = kit.temp_allocator,
      )

      date, date_consumed := time.iso8601_to_time_utc(as_date_string)

      if date_consumed > 0 {
        outer^ = date
        return
      }

      lens__errorf(kit, "Invalid timestamp: '%v'", inner)
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
// {{{ Text
// A sequence of text where all the whitespace in the source is discarded
@(private = "file")
codec__contiguous_text :: proc(k: ^Codec_Kit) -> Typed_Codec(string) {
  return codec__text_impl(k, false)
}

@(private = "file")
codec__text :: proc(k: ^Codec_Kit) -> Typed_Codec(string) {
  return codec__text_impl(k, true)
}

@(private = "file")
codec__text_impl :: proc(
  k: ^Codec_Kit, $spaces: bool
) -> Typed_Codec(string) {
  lens :: proc(kit: ^Lens_Kit) {
    inner := cast(^Exparr(string))kit.inner
    outer := cast(^string)kit.outer

    switch kit.mode {
    case .Project:
      inner.allocator = kit.temp_allocator
      exparr__push(inner, outer^)
    case .Inject:
      size := 0
      for i in 0..<inner.len do size += len(exparr__get(inner^, i))

      // Allocate a string buffer, preventing further re-allocations
      builder := strings.builder_make_len_cap(0, size, kit.allocator)
      builder.buf.allocator = runtime.panic_allocator()

      for i in 0..<inner.len {
        chunk := exparr__get(inner^, i)^
        size := len(builder.buf)
        if chunk == " " && size > 0 && builder.buf[size - 1] == ' ' do continue
        strings.write_string(&builder, chunk)
      }

      outer^ = strings.to_string(builder)
    }
  }

	return codec__memo(
		k,
		"text" when spaces else "contiguous_text",
		proc(k: ^Codec_Kit) -> Typed_Codec(string) {
      when spaces {
        str   := codec__string(k)
        space := codec__space(k, string, " ")
        inner := codec__exparr(k, codec__sum(k, string, str, space))
      } else {
        inner := codec__spaced_exparr(k, codec__string(k))
      }

			return codec__focus(
				k,
				string,
				inner,
        lens,
        scratch = true
			)
		},
	)
}
// }}}
// {{{ Tags
Tag :: distinct string

@(private = "file")
codec__tag :: proc(k: ^Codec_Kit) -> Typed_Codec(Tag) {
  return codec__transmute(k, Tag, codec__contiguous_text(k))
}
// }}}

// {{{ Inline markup
Inline_Markup__Space :: distinct Unit
Inline_Markup__Ellipsis :: distinct Unit
Inline_Markup__Text :: distinct string
Inline_Markup__Emph :: distinct Inline_Markup
Inline_Markup__Strong :: distinct Inline_Markup
Inline_Markup__Strikethrough :: distinct Inline_Markup
Inline_Markup__Mono :: distinct Inline_Markup
Inline_Markup__Quote :: distinct Inline_Markup

Inline_Markup__Icon :: struct {
  id:  string,
  loc: Source_Loc,
  def: ^Def__Icon, // Inserted after the fact
}

Inline_Markup__Fn :: struct {
  id:  string,
  loc: Source_Loc,
  def: ^Def__Footnote, // Inserted after the fact
}

Inline_Markup__Link :: struct {
	id:    string,
	label: Inline_Markup,
  loc:   Source_Loc,
  def:   ^Def__Link, // Inserted after the fact
}

Inline_Markup__Timestamp :: struct {
  time: time.Time,
  compact: bool, // Shortens the output
}

Inline_Markup__Date :: distinct Inline_Markup__Timestamp
Inline_Markup__Datetime :: distinct Inline_Markup__Timestamp

// Using distinct runs into circular types issue (for no reason)
Inline_Markup :: struct {
  // We store a pointer here to drastically reduce the size of the struct. We
  // *could* change every usage site to ^Inline_Markup, but this is simpler for now
	elements: ^Exparr(Inline_Markup__Atom),
}

// TODO: LaTeX
// This currently takes up a whole 32B, which is a bit annoying. We could get
// this as low as 16B by simply using pointers for a bunch of the branches. I
// will bother doing so once the memory usage goes past 1MiB.
Inline_Markup__Atom :: union {
	Inline_Markup__Space,
	Inline_Markup__Ellipsis,
	Inline_Markup__Text,
	Inline_Markup__Emph,
	Inline_Markup__Strong,
	Inline_Markup__Strikethrough,
	Inline_Markup__Mono,
	Inline_Markup__Quote,
	Inline_Markup__Date,
	Inline_Markup__Datetime,
	^Inline_Markup__Icon,
	^Inline_Markup__Fn,
	^Inline_Markup__Link,
}
// }}}
// {{{ Codecs
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

	space := codec__space(k, Inline_Markup__Space, Inline_Markup__Space{})
	text := codec__transmute(k, Inline_Markup__Text, codec__string(k))
	ellipsis := codec__const(k, "...", Inline_Markup__Ellipsis{})
	emph := codec__trans_at(k, "_", Inline_Markup__Emph, imarkup)
	strong := codec__trans_at(k, "*", Inline_Markup__Strong, imarkup)
	strike := codec__trans_at(k, "~", Inline_Markup__Strikethrough, imarkup)
	mono := codec__trans_at(k, "`", Inline_Markup__Mono, imarkup)
	quote := codec__trans_at(k, "\"", Inline_Markup__Quote, imarkup)

  icon_id := codec__field(k, "id", Inline_Markup__Icon, ctext, ONCE)
  icon := codec__ref(k, codec__at(k, "icon", codec__loc(k, icon_id)))

  fn_id := codec__field(k, "id", Inline_Markup__Fn, ctext, ONCE)
  fn := codec__ref(k, codec__at(k, "fn", codec__loc(k, fn_id)))

  Link :: Inline_Markup__Link
  link_id := codec__field(k, "id", Link, ctext, ONCE)
  link_label := codec__field_at(k, "label", Link, imarkup, UNIQUE)
  link_sum := codec__sum(k, Link, link_label, link_id)
  link_payload := codec__loc(k, codec__loop(k, link_sum))
  link := codec__ref(k, codec__at(k, "link", link_payload))

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
          inner := cast(^^Exparr(Inline_Markup__Atom))kit.inner

          found_substantial := false
          for i in 0..<inner^.len {
            elem := exparr__get(inner^^, i)
            if _, ok := elem.(Inline_Markup__Space); !ok {
              found_substantial = true
              break
            }
          }

          if found_substantial {
            mem.copy(kit.outer, kit.inner, size_of(Inline_Markup))
          } else {
            kit.consumed = false
          }
        }
      }

			return codec__focus(
				k,
				Inline_Markup,
        codec__ref(k, codec__exparr(k, codec__inline_markup__atom(k))),
        lens
			)
		},
	)
}
// }}}
// {{{ Formatting as text
inline_markup__atom__fmt :: proc(
  fi: ^fmt.Info, site: Site, page: Page, atom: Inline_Markup__Atom
) {
  switch inner in atom {
  case ^Inline_Markup__Icon, nil:
  case Inline_Markup__Space:
    fmt.wprint(fi.writer, " ")
  case Inline_Markup__Ellipsis:
    fmt.wprint(fi.writer, ELLIPSIS_SYMBOL)
  case Inline_Markup__Text:
    fmt.wprint(fi.writer, string(inner))
  case Inline_Markup__Emph:
    fmt.wprint(fi.writer, "_")
    inline_markup__fmt(fi, site, page, Inline_Markup(inner))
    fmt.wprint(fi.writer, "_")
  case Inline_Markup__Strong:
    fmt.wprint(fi.writer, "*")
    inline_markup__fmt(fi, site, page, Inline_Markup(inner))
    fmt.wprint(fi.writer, "*")
  case Inline_Markup__Strikethrough:
    fmt.wprint(fi.writer, "~")
    inline_markup__fmt(fi, site, page, Inline_Markup(inner))
    fmt.wprint(fi.writer, "~")
  case Inline_Markup__Mono:
    fmt.wprint(fi.writer, "`")
    inline_markup__fmt(fi, site, page, Inline_Markup(inner))
    fmt.wprint(fi.writer, "`")
  case Inline_Markup__Quote:
    fmt.wprint(fi.writer, QUOTE_EN_LEFT)
    inline_markup__fmt(fi, site, page, Inline_Markup(inner))
    fmt.wprint(fi.writer, QUOTE_EN_RIGHT)
  case ^Inline_Markup__Link:
    if mem__non_zero(inner.label) {
      inline_markup__fmt(fi, site, page, inner.label)
    } else if inner.def != nil {
      inline_markup__fmt(fi, site, page, inner.def.label)
    } else {
      fmt.wprint(fi.writer, ERROR_TEXT)
    }
  case ^Inline_Markup__Fn:
    if inner.def != nil {
      fmt.wprintf(fi.writer, "[^%v]", inner.def.index)
    } else {
      fmt.wprint(fi.writer, ERROR_TEXT)
    }
  case Inline_Markup__Date:
    if inner.compact {
      fmt.wprintf(fi.writer, "%v", Date__Compact(inner.time))
    } else {
      fmt.wprintf(fi.writer, "%v", Date__Pretty(inner.time))
    }
  case Inline_Markup__Datetime:
    if inner.compact {
      fmt.wprintf(fi.writer, "%v", Datetime__Compact(inner.time))
    } else {
      fmt.wprintf(fi.writer, "%v", Datetime__Pretty(inner.time))
    }
  }
}

inline_markup__fmt :: proc(
  fi: ^fmt.Info, site: Site, page: Page, im: Inline_Markup
) {
  for i in 0..<im.elements.len {
    chunk := exparr__get(im.elements^, i)^
    inline_markup__atom__fmt(fi, site, page, chunk)
  }
}

inline_markup__formatter :: proc(
  site: ^Site, page: ^Page, im: ^Inline_Markup
) -> Frozen {
  return fmt__freeze3(
    site, 
    page, 
    im,
    proc(fi: ^fmt.Info, site: ^Site, page: ^Page, im: ^Inline_Markup) {
      inline_markup__fmt(fi, site^, page^, im^)
    },
  )
}
// }}}
// {{{ Formatting as html
inline_markup__atom__html :: proc(
  site: ^Site, page: Page, atom: Inline_Markup__Atom
) {
  g := &site.xml
  switch inner in atom {
  case nil:
  case ^Inline_Markup__Icon:
    xml__attr(g, "class", "icon")
    // Decorative image.
    // See: https://www.w3.org/WAI/tutorials/images/decorative/
    xml__attr(g, "alt", "")
    xml__attr(g, "src", inner.def.path)
  case Inline_Markup__Space:
    xml__string(g, " ")
  case Inline_Markup__Ellipsis:
    xml__stringf(g, "%v", ELLIPSIS_SYMBOL)
  case Inline_Markup__Text:
    xml__string(g, string(inner))
  case Inline_Markup__Emph:
    xml__tag(g, "em")
    inline_markup__html(site, page, Inline_Markup(inner))
  case Inline_Markup__Strong:
    xml__tag(g, "strong")
    inline_markup__html(site, page, Inline_Markup(inner))
  case Inline_Markup__Strikethrough:
    xml__tag(g, "s")
    inline_markup__html(site, page, Inline_Markup(inner))
  case Inline_Markup__Mono:
    xml__tag(g, "code")
    inline_markup__html(site, page, Inline_Markup(inner))
  case Inline_Markup__Quote:
    xml__stringf(g, "%v", QUOTE_EN_LEFT)
    inline_markup__html(site, page, Inline_Markup(inner))
    xml__stringf(g, "%v", QUOTE_EN_RIGHT)
  case ^Inline_Markup__Link:
    xml__tag(g, "a")
    xml__attr(g, "href", inner.def.target)
    if mem__non_zero(inner.label) {
      inline_markup__html(site, page, inner.label)
    } else if inner.def != nil {
      inline_markup__html(site, page, inner.def.label)
    } else {
      xml__string(g, ERROR_TEXT)
    }
  case ^Inline_Markup__Fn:
    xml__tag(g, "sup")
    xml__tag(g, "a")
    xml__attr(g, "role", "doc-noteref")

    if inner.def != nil {
      xml__attrf(g, "id", "footnote-reference-%v", inner.def.index)
      xml__attrf(g, "href", "#footnote-%v", inner.def.index)
      xml__stringf(g, "%v", inner.def.index)
    } else {
      xml__string(g, ERROR_TEXT)
    }
  case Inline_Markup__Date:
    xml__tag(g, "time")
    xml__attrf(g, "datetime", "%v", Rfc3339(inner.time))
    if inner.compact {
      xml__stringf(g, "%v", Date__Compact(inner.time))
    } else {
      xml__stringf(g, "%v", Date__Pretty(inner.time))
    }
  case Inline_Markup__Datetime:
    xml__tag(g, "time")
    xml__attrf(g, "datetime", "%v", Rfc3339(inner.time))
    if inner.compact {
      xml__stringf(g, "%v", Datetime__Compact(inner.time))
    } else {
      xml__stringf(g, "%v", Datetime__Pretty(inner.time))
    }
  }
}

inline_markup__html :: proc(
  site: ^Site, page: Page, im: Inline_Markup
) {
  for i in 0..<im.elements.len {
    chunk := exparr__get(im.elements^, i)^
    inline_markup__atom__html(site, page, chunk)
  }
}
// }}}
// {{{ Checking
inline_markup__check :: proc(site: ^Site, page: Page, im: ^Inline_Markup) {
  if im.elements == nil do return
  // TODO: remove spirious space here
  for i in 0..<im.elements.len {
    chunk := exparr__get(im.elements^, i)
    inline_markup__atom__check(site, page, chunk)
  }
}

inline_markup__atom__check :: proc(
  site: ^Site, page: Page, atom: ^Inline_Markup__Atom
) {
  switch &inner in atom {
  case nil:
  case Inline_Markup__Space:
  case Inline_Markup__Ellipsis:
  case Inline_Markup__Text:
  case Inline_Markup__Date: 
  case Inline_Markup__Datetime: 
  case Inline_Markup__Emph:
    inline_markup__check(site, page, cast(^Inline_Markup)&inner)
  case Inline_Markup__Strong:
    inline_markup__check(site, page, cast(^Inline_Markup)&inner)
  case Inline_Markup__Strikethrough:
    inline_markup__check(site, page, cast(^Inline_Markup)&inner)
  case Inline_Markup__Mono:
    inline_markup__check(site, page, cast(^Inline_Markup)&inner)
  case Inline_Markup__Quote:
    inline_markup__check(site, page, cast(^Inline_Markup)&inner)
  case ^Inline_Markup__Link:
    log.assert(inner.def == nil)

    if mem__non_zero(inner.label) {
      inline_markup__check(site, page, &inner.label)
    }

    for i in 0..<site.pages.len {
      defsite := exparr__get(site.pages, i)
      for j in 0..<defsite.links.len {
        link := exparr__get(defsite.links, j)
        (link.id == inner.id) or_continue
        page_filter__all__eval(defsite^, page, link.scope) or_continue
        inner.def = link
        return
      }
    }

    site__errorf(
      site,
      inner.loc,
      "Link '%v' is not in scope.",
      inner.id
    )
  case ^Inline_Markup__Fn:
    log.assert(inner.def == nil)
    for j in 0..<page.footnotes.len {
      footnote := exparr__get(page.footnotes, j)
      (footnote.id == inner.id) or_continue
      inner.def = footnote
      return
    }

    site__errorf(
      site,
      inner.loc,
      "Footnote '%v' is not in scope.",
      inner.id
    )
  case ^Inline_Markup__Icon:
    log.assert(inner.def == nil)
    for i in 0..<site.pages.len {
      defsite := exparr__get(site.pages, i)
      for j in 0..<defsite.icons.len {
        icon := exparr__get(defsite.icons, j)
        (icon.id == inner.id) or_continue
        page_filter__all__eval(defsite^, page, icon.scope) or_continue
        inner.def = icon
        return
      }
    }

    site__errorf(
      site,
      inner.loc,
      "Icon '%v' is not in scope.",
      inner.id
    )
  }
}

// }}}

// {{{ Block markup
Block_Markup__Paragraph :: distinct Inline_Markup

Block_Markup__Image :: struct {
	alt:    Inline_Markup,
	source: string,
}

Block_Markup__Figure :: struct {
	caption: Inline_Markup,
	content: Block_Markup,
}

Block_Markup__List :: struct {
	ordered:  bool,
	block:    bool,
	using elements: struct #raw_union {
		imarkup: Exparr(Inline_Markup),
    bmarkup: Exparr(Block_Markup),
	},
}

Block_Markup__Aside :: struct {
  id:       string,
  char:     string, // Icon name
  content:  Block_Markup,
  title:    Inline_Markup,

  // Whether to hide the content by default
  collapse: bool,
}

Block_Markup__Code :: struct {
  language: string,
  content:  string,
}

Block_Markup__Blockquote :: distinct Block_Markup
Block_Markup__Description :: distinct Unit
Block_Markup__Table_Of_Contents :: distinct Unit
Block_Markup__Thematic_Break :: distinct Unit
Block_Markup__Index :: distinct Page_Filter__All

// These are inserted after the fact, during the checking phase
Block_Markup__Section :: struct {
  heading: ^Heading,
  content: Block_Markup,
}

// TODO: code, list
// This currently takes up a fat 144B. If memory usage ever goes past 1MiB, I
// will bother using pointers for the various branches, thus not wasting so much
// space on padding.
Block_Markup__Atom :: union {
	Block_Markup__Paragraph,
	Block_Markup__Image,
	Block_Markup__Figure,
	Block_Markup__List,
	Block_Markup__Blockquote,
	Block_Markup__Description,
	Block_Markup__Table_Of_Contents,
	Block_Markup__Thematic_Break,
  Block_Markup__Index,
  Block_Markup__Aside,
  Block_Markup__Code,
  Block_Markup__Section,
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
// {{{ Codecs
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

	h2 := codec__at(k, "#", codec__heading(k, 2))
	h3 := codec__at(k, "##", codec__heading(k, 3))
	h4 := codec__at(k, "###", codec__heading(k, 4))

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
		codec__variant(k, Block_Markup__Atom, h2),
		codec__variant(k, Block_Markup__Atom, h3),
		codec__variant(k, Block_Markup__Atom, h4),
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
// {{{ Formatting as html
@(private="file")
block_markup__anchored_heading :: proc(
  site: ^Site, page: Page, heading: Heading
) {
  @(static, rodata)
  TAG_NAMES: [MAX_HEADING_LEVEL]string = {"h1", "h2", "h3", "h4"}

  g := &site.xml
  xml__tag(g, TAG_NAMES[heading.level - 1])
  xml__attr(g, "id", heading.id)

  if xml__tag(g, "a") {
    xml__attr(g, "class", "heading-anchor")

    if mem__non_zero(heading.id) {
      xml__attrf(g, "href", "#%v", heading.id)
    } else {
      xml__attr(g, "href", "")
    }

    xml__string(g, "◇")
  }

  xml__string(g, " ")
  inline_markup__html(site, page, heading.content)
}

block_markup__atom__html :: proc(
  site: ^Site, page: Page, atom: Block_Markup__Atom
) {
  g := &site.xml
  #partial switch inner in atom {
  case nil:
  case ^Def__Link:
  case ^Def__Footnote:
  case ^Def__Icon:
  case Block_Markup__Thematic_Break:
    xml__tag(g, "hr", single = true)
  case ^Heading:
    log.panic("Cannot render section-less heading as HTML")
  case Block_Markup__Description:
    xml__tag(g, "p")
    inline_markup__html(site, page, page.description)
  case Block_Markup__Table_Of_Contents: // TODO
    xml__tag(g, "details")
    if xml__tag(g, "summary") do xml__string(g, "Toggle table of contens")
    xml__tag(g, "nav")
    xml__attr(g, "role", "doc-toc")
    xml__attr(g, "aria_labelledby", "toc-title")
    if xml__tag(g, "h3") {
      xml__attr(g, "id", "toc-title")
      xml__string(g, "Table of Contents")
    }

    xml__tag(g, "ol")
    stack: small_array.Small_Array(MAX_HEADING_LEVEL, uint)
    // Whether we've created an <ol> element for the top of the stack. Since an
    // empty stack is contained in the <ol> we've just created above, this
    // starts out as being true.
    last_has_children := true
    for i in 0..<page.headings.len {
      heading := exparr__get(page.headings, i)

      for j := small_array.len(stack) - 1; j >= 0; j -= 1 {
        last := small_array.get(stack, j)
        (last >= heading.level) or_break
        small_array.pop_back(&stack)
        if last_has_children do xml__tag_end(g) // </ol>
        xml__tag_end(g) // </li>
        last_has_children = true
      }

      if !last_has_children {
        xml__tag(g, "ol", auto_close = false)
      }

      small_array.push_back(&stack, heading.level)
      xml__tag(g, "li", auto_close = false)
      xml__tag(g, "a")
      xml__attrf(g, "href", "#%v", heading.id)
      inline_markup__html(site, page, heading.content)
      last_has_children = false
    }

    // Clean what's left of the stack
    for small_array.len(stack) > 0 {
      small_array.pop_back(&stack)
      if last_has_children do xml__tag_end(g) // </ol>
      xml__tag_end(g) // </li>
      last_has_children = true
    }
  case Block_Markup__Index: // TODO
  case Block_Markup__Section:
    xml__tag(g, "section")
    xml__attr(g, "aria-labelledby", inner.heading.id)
    block_markup__anchored_heading(site, page, inner.heading^)
    block_markup__html(site, page, inner.content)
  case Block_Markup__Paragraph:
    xml__tag(g, "p")
    inline_markup__html(site, page, Inline_Markup(inner))
  case Block_Markup__Blockquote:
    xml__tag(g, "blockquote")
    block_markup__html(site, page, Block_Markup(inner))
  case Block_Markup__List:
    xml__tag(g, inner.ordered ? "ol" : "ul")
    if inner.block {
      for i in 0..<inner.bmarkup.len {
        xml__tag(g, "li")
        block_markup__html(site, page, exparr__get(inner.bmarkup, i)^)
      }
    } else {
      for i in 0..<inner.imarkup.len {
        xml__tag(g, "li")
        inline_markup__html(site, page, exparr__get(inner.imarkup, i)^)
      }
    }
  case Block_Markup__Aside: // TODO
  }
}

block_markup__html :: proc(
  site: ^Site, page: Page, bm: Block_Markup
) {
  for i in 0..<bm.elements.len {
    chunk := exparr__get(bm.elements, i)^
    block_markup__atom__html(site, page, chunk)
  }
}
// }}}
// {{{ Checking
block_markup__check :: proc(site: ^Site, page: Page, bm: ^Block_Markup) {
  if bm == nil do return
  for i in 0..<bm.elements.len {
    atom := exparr__get(bm.elements, i)
    block_markup__atom__check(site, page, atom)
  }

  // Split block into sections We don't attempt to reuse any of the existing
  // structure... As such, we try to avoind neednessly creating new copies when
  // the body contains no headings.
  has_headings: bool
  for i in 0..<bm.elements.len {
    atom := exparr__get(bm.elements, i)
    heading := atom.(^Heading) or_continue
    has_headings = true
    break
  }
  if !has_headings do return

  allocator := virtual.arena_allocator(&site.forever_arena)
  sectioned: Block_Markup // The root section we write to
  sectioned.elements.allocator = allocator

  stack: small_array.Small_Array(MAX_HEADING_LEVEL, ^Block_Markup__Section)
  for i in 0..<bm.elements.len {
    atom := exparr__get(bm.elements, i)

    if heading, ok := atom.(^Heading); ok {
      for j := small_array.len(stack) - 1; j >= 0; j -= 1 {
        last := small_array.get(stack, j)
        (last.heading.level >= heading.level) or_break
        small_array.pop_back(&stack)
      }

      inner_content: Block_Markup
      inner_content.elements.allocator = allocator
      atom := Block_Markup__Section {
        heading = heading,
        content = inner_content,
      }

      ref: ^Block_Markup__Atom
      if l := small_array.len(stack); l > 0 {
        last := small_array.get(stack, l - 1)
        ref = exparr__push(&last.content.elements, atom)
      } else {
        ref = exparr__push(&sectioned.elements, atom)
      }

      section := &ref.(Block_Markup__Section)
      log.assert(small_array.push_back(&stack, section))
    } else if l := small_array.len(stack); l > 0 {
      last := small_array.get(stack, l - 1)
      exparr__push(&last.content.elements, atom^)
    } else {
      exparr__push(&sectioned.elements, atom^)
    }
  }

  bm^ = sectioned
}

block_markup__atom__check :: proc(
  site: ^Site, page: Page, atom: ^Block_Markup__Atom
) {
  switch &inner in atom {
  case nil:
  case Block_Markup__Code:
  case Block_Markup__Description:
  case Block_Markup__Table_Of_Contents:
  case Block_Markup__Index:
  case Block_Markup__Thematic_Break:
  case ^Def__Link:
  case ^Def__Footnote:
  case ^Def__Icon:
  case ^Heading:
  case Block_Markup__Section:
    block_markup__check(site, page, &inner.content)
  case Block_Markup__Paragraph:
    inline_markup__check(site, page, cast(^Inline_Markup)&inner)
  case Block_Markup__Image:
    inline_markup__check(site, page, &inner.alt)
  case Block_Markup__Figure:
    inline_markup__check(site, page, &inner.caption)
    block_markup__check(site, page, &inner.content)
  case Block_Markup__List:
    if inner.block {
    	for i in 0 ..< inner.bmarkup.len {
    		block_markup__check(site, page, exparr__get(inner.bmarkup, i))
    	}
    } else {
    	for i in 0 ..< inner.imarkup.len {
    		inline_markup__check(site, page, exparr__get(inner.imarkup, i))
    	}
    }
  case Block_Markup__Aside:
    inline_markup__check(site, page, &inner.title)
    block_markup__check(site, page, &inner.content)
  case Block_Markup__Blockquote:
    block_markup__check(site, page, cast(^Block_Markup)&inner)
  case Table:
    table__check(site, page, &inner)
  }
}
// }}}
