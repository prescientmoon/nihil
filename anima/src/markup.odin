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

  filename:    string, // Overrides the last segment of the path
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
  headings:  Exparr(Heading),
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

page__title :: proc(page: Page) -> Heading {
  if page.headings.len == 0 do return {}
  return exparr__get(page.headings, 0)^
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

// This currently performs a linear search. In case performance ever becomes an
// issue, I'll add a hashmap on top.
//
// TODO: ensure no link shadows an existing definition. This requires keeping
// tack of spans when paring though... My idea for doing that is exposing the 
// location of the parent apparition token to lenses.
site__lookup_link :: proc(site: Site, page: Page, id: string) -> (result: ^Def__Link) {
  for i in 0..<site.pages.len {
    page := exparr__get(site.pages, i)
    for j in 0..<page.links.len {
      link := exparr__get(page.links, j)
      (link.id == id) or_continue
      return result
    }
  }

  return nil
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
Heading :: struct {
  id:      string,
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
	return codec__remote_push(k, "headings", Page, with_level)
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
  def: ^Def__Icon, // Inserted after the fact
}

Inline_Markup__Fn :: struct {
  id:  string,
  def: ^Def__Footnote, // Inserted after the fact
}

Inline_Markup__Link :: struct {
	id:    string,
	label: Inline_Markup,
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
	Inline_Markup__Icon,
	Inline_Markup__Fn,
	Inline_Markup__Date,
	Inline_Markup__Datetime,
	Inline_Markup__Link,
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
  icon := codec__at(k, "icon", icon_id)

  fn_id := codec__field(k, "id", Inline_Markup__Fn, ctext, ONCE)
  fn := codec__at(k, "fn", fn_id)

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
	elements: union #no_nil {
		Exparr(Inline_Markup),
		Exparr(Block_Markup),
	},
}

Block_Markup__Aside :: struct {
  id:       string,
  char:     string,
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

ELLIPSIS_SYMBOL :: '…'
QUOTE_EN_LEFT   :: '“'
QUOTE_EN_RIGHT  :: '”'

// Inserted in the page when errors are encountered
ERROR_TEXT  :: "🚨 ERROR 🚨"

// Conversion
inline_markup__atom__to_text :: proc(
  site: ^Site, page: Page, atom: Inline_Markup__Atom
) {
  switch inner in atom {
  case Inline_Markup__Icon, nil:
  case Inline_Markup__Space:
    fmt.sbprint(&site.txt, " ")
  case Inline_Markup__Ellipsis:
    fmt.sbprint(&site.txt, ELLIPSIS_SYMBOL)
  case Inline_Markup__Text:
    fmt.sbprint(&site.txt, string(inner))
  case Inline_Markup__Emph:
    fmt.sbprint(&site.txt, "_")
    inline_markup__to_text(site, page, Inline_Markup(inner))
    fmt.sbprint(&site.txt, "_")
  case Inline_Markup__Strong:
    fmt.sbprint(&site.txt, "*")
    inline_markup__to_text(site, page, Inline_Markup(inner))
    fmt.sbprint(&site.txt, "*")
  case Inline_Markup__Strikethrough:
    fmt.sbprint(&site.txt, "~")
    inline_markup__to_text(site, page, Inline_Markup(inner))
    fmt.sbprint(&site.txt, "~")
  case Inline_Markup__Mono:
    fmt.sbprint(&site.txt, "`")
    inline_markup__to_text(site, page, Inline_Markup(inner))
    fmt.sbprint(&site.txt, "`")
  case Inline_Markup__Quote:
    fmt.sbprint(&site.txt, QUOTE_EN_LEFT)
    inline_markup__to_text(site, page, Inline_Markup(inner))
    fmt.sbprint(&site.txt, QUOTE_EN_RIGHT)
  case Inline_Markup__Link:
    if mem__non_zero(inner.label) {
      inline_markup__to_text(site, page, inner.label)
    } else if inner.def != nil {
      inline_markup__to_text(site, page, inner.def.label)
    } else {
      fmt.sbprint(&site.txt, ERROR_TEXT)
    }
  case Inline_Markup__Fn:
    if inner.def != nil {
      fmt.sbprintf(&site.txt, "[^%v]", inner.def.index)
    } else {
      fmt.sbprint(&site.txt, ERROR_TEXT)
    }
  case Inline_Markup__Date: // TODO
    fmt.sbprint(&site.txt, ERROR_TEXT)
  case Inline_Markup__Datetime: // TODO
    fmt.sbprint(&site.txt, ERROR_TEXT)
  }
}

inline_markup__to_text :: proc(site: ^Site, page: Page, im: Inline_Markup) {
  for i in 0..<im.elements.len {
    chunk := exparr__get(im.elements^, i)^
    inline_markup__atom__to_text(site, page, chunk)
  }
}
