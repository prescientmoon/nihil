package anima

import "core:fmt"
import "core:log"
import "core:mem"
import "core:mem/virtual"
import "core:strconv"
import "core:strings"
import "core:time"
import "core:unicode"

// {{{ Page
Page :: struct {
  compact:          bool, // Whether the page should not contain the post layout
  public:           bool, // Whether the page should be included in the sitemap
  smaller_headings: bool,

  filename:    string, // Overrides the last segment of the path
  title:       Inline_Markup,
  description: Inline_Markup,
  content:     Block_Markup,

  created_at:   time.Time, // When was the file created?
  published_at: time.Time, // When was the file created?

  // We don't bother defining sitemap-specific types since those options are
  // seldom used in practice.
  changefreq: string, // Override the change frequency for the page
  priority:   string, // Assign a priority to the sitemap entry

  tags:      Exparr(Tag),
  changelog: Exparr(Change),
  headings:  Exparr(Heading), // The first heading is declared the title
	links:     Exparr(Def__Link),
	icons:     Exparr(Def__Icon),
  feeds:     Exparr(Def__Feed),
	footnotes: Exparr(Def__Footnote),
  styles:    Exparr(Def__Stylesheet),
  assets:    Exparr(Def__Asset),
  aliases:   Exparr(Path__Output), // Locations to redirect from

  // These attributes are not part of the markup itself
  source_path: Path__Input,
  site_path:   Path__Output,
  url:         URL,
  word_count:  uint, // Merely an estimate!
  in_feeds:    Exparr(^Def__Feed),
  in_styles:   Exparr(^Def__Stylesheet),
}

page__make :: proc(allocator: mem.Allocator) -> (page: Page) {
  page.links.allocator     = allocator
  page.icons.allocator     = allocator
  page.headings.allocator  = allocator
  page.footnotes.allocator = allocator
  page.styles.allocator    = allocator
  page.in_feeds.allocator  = allocator
  page.in_styles.allocator = allocator
  return page
}

page__last_updated :: proc(page: Page) -> (t: time.Time) {
  t = time__max(page.created_at, page.published_at)
  for iter := iter__mk(page.changelog); change in iter__next(&iter) {
    t = time__max(t, change.at)
  }

  return t
}

// When a page moves, we want to keep using the old URL as the RSS GUID.
page__guid :: proc(
  site: ^Site, page: Page, alloc: Site_Alloc = .Forever
) -> URL {
  if page.aliases.len == 0 {
    return page.url
  } else {
    first := get(page.aliases, 0)
    return site__url(site, first^, alloc)
  }
}

codec__page :: proc(k: ^Codec_Kit) -> ^Codec {
  ctext      := codec__contiguous_text(k)
  imarkup    := codec__inline_markup(k)
  bmarkup    := codec__block_markup(k)
  timestamp  := codec__timestamp(k)
  stylesheet := codec__stylesheet(k)
  out_path   := codec__out_path(k)

  feeds_payload   := codec__exparr(k, codec__at(k, "feed",       codec__feed(k)))
  tags_payload    := codec__exparr(k, codec__at(k, "tag",        codec__tag(k)))
  changes_payload := codec__exparr(k, codec__at(k, "change",     codec__change(k)))
  aliases_payload := codec__exparr(k, codec__at(k, "alias",      out_path))
  styles_payload  := codec__exparr(k, codec__at(k, "stylesheet", stylesheet))
  assets_payload  := codec__exparr(k, codec__at(k, "asset",      codec__asset(k)))

  feeds   := codec__field(k, "feeds",     Page, feeds_payload)
  tags    := codec__field(k, "tags",      Page, tags_payload)
  changes := codec__field(k, "changelog", Page, changes_payload)
  aliases := codec__field(k, "aliases",   Page, aliases_payload)
  styles  := codec__field(k, "styles",    Page, styles_payload)
  assets  := codec__field(k, "assets",    Page, assets_payload)

  content     := codec__field(k, "content", Page, bmarkup, REQUIRED)
  title       := codec__field_at(k, "title", Page, imarkup, ONCE)
  description := codec__field_at(k, "description", Page, imarkup, UNIQUE)
  filename    := codec__field_at(k, "filename", Page, ctext, UNIQUE)

  created   := codec__at(
    k, "created-at", codec__field(k, "created_at", Page, timestamp), ONCE
  )

  published := codec__at(
    k, "published-at", codec__field(k, "published_at", Page, timestamp), UNIQUE
  )

  priority   := codec__field_at(k, "priority",   Page, ctext, UNIQUE)
  changefreq := codec__field_at(k, "changefreq", Page, ctext, UNIQUE)

  compact := codec__flag_at(k, "compact", Page)
  public  := codec__flag_at(k, "public", Page)
  smaller_headings := codec__field(
    k, "smaller_headings", Page, codec__flag(k, "smaller-headings")
  )

  inner_loop := codec__sum(
    k,
    feeds, tags, aliases, styles, assets, public, title, description, compact,
    smaller_headings, created, published, filename, changefreq, priority,
    changes, content,
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
// {{{ Formatting as html
Page_Gen_Mode :: enum {
  Self,
  Changelog,
}

page__html :: proc(g: ^Xml_Gen, page: ^Page, mode: Page_Gen_Mode) {
  xml__raw_string(g, "<!doctype html>")
  xml__tag(g, "html")
  xml__attr(g, "lang", "en")
  if xml__tag(g, "head") {
    if xml__tag(g, "meta", true) do xml__attr(g, "charset", "utf-8")

    meta :: proc(g: ^Xml_Gen, prop_name: string, prop: string, content: any) {
      if xml__tag(g, "meta", true) {
        xml__attrf(g, prop_name, "%v", prop)
        xml__attrf(g, "content", "%v", content)
      }
    }

    meta(g, "name", "theme-color", THEME_COLOR)
    meta(
      g, "name", "viewport",
      "width=device-width, initial-scale=1, maximum-scale=1, shrink-to-fit=no"
    )

    title := inline_markup__formatter(g.site, page, &page.title)
    desc  := inline_markup__formatter(g.site, page, &page.description)
    meta(g, "property", "og:site_name", SITE_NAME)
    meta(g, "property", "og:url", page.url)
    meta(g, "property", "og:title", title)
    meta(g, "property", "og:description", desc)
    meta(g, "property", "twitter:title", title)
    meta(g, "property", "twitter:description", desc)
    meta(g, "name", "fediverse:creator", FEDI_USER)

    for iter := iter__mk(page.in_feeds); feed in iter__next(&iter) {
      feed := feed^
      xml__tag(g, "link")
      xml__attr(g, "rel", "alternate")
      xml__attr(g, "type", "application/rss+xml")
      xml__attrf(g, "title", "Moonythm | %v", strings.trim_space(feed.name))
      xml__attr(g, "href", site__url(g.site, feed.site_path, .Stack))
    }

    for iter := iter__mk(page.in_styles); style in iter__next(&iter) {
      style := style^
      if style.preload {
        xml__tag(g, "link")
        xml__attr(g, "rel", "preload")
        xml__attr(g, "as", "stylesheet")
        xml__attr(g, "href", site__url(g.site, style.site_path, .Stack))
      }

      xml__tag(g, "link")
      xml__attr(g, "rel", "stylesheet")
      xml__attr(g, "href", site__url(g.site, style.site_path, .Stack))
    }

    if xml__tag(g, "title") do inline_markup__html(g, page^, page.title)

    // NOTE: I might un-hardcode these paths in the future
    if mem__non_zero(g.site.favicon) {
      xml__tag(g, "link")
      xml__attr(g, "rel", "icon")
      xml__attr(g, "type", "image/x-icon")
      xml__attr(g, "href", site__url(g.site, g.site.favicon.site_path, .Stack))
    }

    if xml__tag(g, "link") {
      xml__attr(g, "rel", "preload")
      xml__attr(g, "as", "font")
      xml__attr(g, "href", "/fonts/computer-modern/cm-regular.woff2")
      xml__attr(g, "type", "font/woff2")
      xml__flag(g, "crossorigin") // Required when as=font
    }
  }

  if xml__tag(g, "body") {
    if xml__tag(g, "header") {
      // TODO: think about what to put here a bit further
      if xml__tag(g, "a") {
        xml__attr(g, "href", "/")
        xml__tag(g, "code")
        xml__string(g, "~")
      }
    }

    switch mode {
    case .Self:
      xml__tag(g, "main")
      if page.smaller_headings do xml__attr(g, "class", "smaller-headings")
      xml__attr(g, "aria-labelledby", "main")
      heading := Heading { level = 1, content = page.title, id = "main" } 
      if page.compact {
        block_markup__anchored_heading(g, page^, heading, main = true)
      } else if xml__tag(g, "header") {
        block_markup__anchored_heading(g, page^, heading, main = true)

        // NOTE: we do duplicate these, so I might eventually abstract them away
        if xml__tag(g, "ul") {
          if xml__tag(g, "li") {
            xml__stringf(g, "%v by ", fmt__posted_on(&page.published_at))
            xml__tag(g, "a")
            xml__attr(g, "href", g.site.base_url)
            xml__attr(g, "rel", "bookmark")
            xml__string(g, USERNAME)
          }

          if xml__tag(g, "li") {
            at := page__last_updated(page^)
            xml__stringf(g, "Last updated on %v", Datetime__Pretty(at))
          }

          if xml__tag(g, "li") {
            at := page__last_updated(page^)
            xml__stringf(
              g, "About %v words; a %v read",
              fmt__word_count(&page.word_count),
              fmt__reading_duration(&page.word_count),
            )
          }
        }

        xml__tag(g, "hr")
      }

      if page.compact {
        block_markup__html(g, page^, page.content)
      } else {
        xml__tag(g, "article")
        block_markup__html(g, page^, page.content)
      }

      // TODO: footnotes
    case .Changelog: // TODO
    }
  }
}
// }}}
// {{{ Checking
page__check :: proc(site: ^Site, page: ^Page) {
  // TODO: apply the filename option
  page.site_path = Path__Output(page.source_path)
  page.url = site__url(site, page.site_path)

  inline_markup__check(site, page, &page.description)
  block_markup__check(site, page, &page.content)

  for iter := iter__mk(page.footnotes); footnote in iter__next(&iter) {
    block_markup__check(site, page, &footnote.content)
  }

  for iter := iter__mk(page.links); link in iter__next(&iter) {
    inline_markup__check(site, page, &link.label)
  }

  // Collect all the styles/feeds affecting the page
  for iter := iter__mk(site.pages); other_page in iter__next(&iter) {
    for iter := iter__mk(other_page.feeds); feed in iter__next(&iter) {
      page_filter__all__eval(other_page^, page^, feed.under) or_continue
      push(&page.in_feeds, feed)
    }

    for iter := iter__mk(other_page.styles); style in iter__next(&iter) {
      page_filter__all__eval(other_page^, page^, style.scope) or_continue
      push(&page.in_styles, style)
    }
  }

  // Generate feed paths & redirects
  for iter := iter__mk(page.feeds); feed in iter__next(&iter) {
    feed.site_path = site__resolve(site, page.site_path, feed.at)
    for iter := iter__mk(feed.aliases); alias in iter__next(&iter) {
      push(&site.redirects, Redirect{alias^, feed.site_path})
    }
  }

  // Generate stylesheet paths
  for iter := iter__mk(page.styles); style in iter__next(&iter) {
    style.site_path = site__resolve(site, page.site_path, style.at)
  }

  // Generate icon paths
  for iter := iter__mk(page.icons); icon in iter__next(&iter) {
    icon.site_path = site__resolve(site, page.site_path, icon.at)
    if icon.favicon {
      if mem__is_zero(site.favicon) {
        site.favicon = icon
      } else {
        site__errorf(
          site,
          icon.loc,
          "Favicon already set at %v.",
          site.favicon.loc,
        )
      }
    }
  }

  // Set up alias redirects
  for iter := iter__mk(page.aliases); alias in iter__next(&iter) {
    push(&site.redirects, Redirect{alias^, page.site_path})
  }

  // Set defaults for asset paths
  for iter := iter__mk(page.assets); asset in iter__next(&iter) {
    if mem__is_zero(asset.to) {
      asset.to = asset.from
    }
  }

  level: uint = 1 // the title is equivalent to a h1
  for iter := iter__mk(page.headings); heading in iter__next(&iter) {
    inline_markup__check(site, page, &heading.content)

    if heading.level > level + 1 {
      site__errorf(site, heading.loc, "Heading increases level by more than 1")
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

codec__change :: proc(k: ^Codec_Kit) -> ^Codec {
  imarkup := codec__inline_markup(k)
  at := codec__field_at(k, "at", Change, codec__timestamp(k), ONCE)
  message := codec__field(k, "message", Change, imarkup, REQUIRED)

  return codec__loop(k, codec__sum(k, at, message))
}
// }}}
// {{{ Page filtering
// We make this a struct, since we otherwise hit bugs in the compiler
Page_Filter__Many :: struct { elements: Exparr(Page_Filter__Atom) }

Page_Filter__Any   :: distinct Page_Filter__Many  // OR
Page_Filter__All   :: distinct Page_Filter__Many  // AND
Page_Filter__Not   :: distinct ^Page_Filter__Atom // NOT

Page_Filter__Tag    :: distinct Tag  // Pages having this tag
Page_Filter__Local  :: distinct Unit // The current page
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

// Behaves like \local when empty and like \all otherwise
Page_Filter__Scope :: distinct Page_Filter__All

page_filter__eval :: proc(base, page: Page, filter: Page_Filter__Atom) -> bool {
  switch inner in filter {
  case Page_Filter__Not: 
    if inner == nil do return false
    return !page_filter__eval(base, page, inner^)
  case Page_Filter__Public: return page.public
  case Page_Filter__All: return page_filter__all__eval(base, page, inner)
  case Page_Filter__Local: return page.site_path == base.site_path
  case Page_Filter__Any: 
    for iter := iter__mk(inner.elements); filter in iter__next(&iter) {
      if page_filter__eval(base, page, filter^) do return true
    }

    return false
  case Page_Filter__Tag: 
    goal := Tag(inner)
    for iter := iter__mk(page.tags); tag in iter__next(&iter) {
      if goal == tag^ do return true
    }

    return false
  }

  log.panic("impossible")
}

page_filter__all__eval :: proc(base, page: Page, all: Page_Filter__All) -> bool {
  for iter := iter__mk(all.elements); filter in iter__next(&iter) {
    if !page_filter__eval(base, page, filter^) do return false
  }

  return true
}

page_filter__scope__eval :: proc(
  base, page: Page, scope: Page_Filter__Scope
) -> bool {
  if scope.elements.len == 0 {
    return page_filter__eval(base, page, Page_Filter__Local{})
  } else {
    return page_filter__all__eval(base, page, Page_Filter__All(scope))
  }
}

@(private = "file")
codec__page_filter__atom :: proc(
  k: ^Codec_Kit
) -> ^Codec {
  return codec__memo(
  	k,
  	"page_filter__atom",
    Page_Filter__Atom,
  	proc(k: ^Codec_Kit) -> ^Codec {
      atom := codec__page_filter__atom(k)
      many := codec__page_filter__many(k)

      local := codec__const(k, "local", Page_Filter__Local{})
      public := codec__const(k, "public", Page_Filter__Public{})
      not := codec__trans_at(k, "not", Page_Filter__Not, codec__ref(k, atom))
      all := codec__trans_at(k, "all", Page_Filter__All, many, {}, {})
      any := codec__trans_at(k, "any", Page_Filter__Any, many, {}, {})
      tag := codec__trans_at(k, "tag", Page_Filter__Tag, codec__tag(k))

      return codec__union(
        k,
        Page_Filter__Atom,
        { Page_Filter__Local,  local  },
        { Page_Filter__Public, public },
        { Page_Filter__Not,    not    },
        { Page_Filter__All,    all    },
        { Page_Filter__Any,    any    },
        { Page_Filter__Tag,    tag    },
      )
    },
  )
}

@(private = "file")
codec__page_filter__many :: proc(
  k: ^Codec_Kit
) -> ^Codec {
	return codec__memo(
		k,
		"page_filter__many",
    Page_Filter__Many,
		proc(k: ^Codec_Kit) -> ^Codec {
      inner := codec__spaced_exparr(k, codec__page_filter__atom(k))
			return codec__transmute(k, Page_Filter__Many, inner)
		},
	)
}

@(private = "file")
codec__page_filter__all :: proc(
  k: ^Codec_Kit
) -> ^Codec {
	return codec__memo(
		k,
		"page_filter__all",
    Page_Filter__All,
		proc(k: ^Codec_Kit) -> ^Codec {
			return codec__transmute(k, Page_Filter__All, codec__page_filter__many(k))
		},
	)
}
// }}}
// {{{ Icon definitions
Def__Icon :: struct {
  id:    string,
  at:    Path,
  scope: Page_Filter__Scope,

  // When set to true, makes this the website's favicon. Favicons are currently
  // global and cannot be set on a per-page basis.
  favicon: bool,

  // Generated
  loc:       Source_Loc,
  site_path: Path__Output,
}

@(private = "file")
codec__deficon :: proc(k: ^Codec_Kit) -> ^Codec {
  Self :: Def__Icon

	id := codec__field(k, "id", Self, codec__contiguous_text(k), REQUIRED)
	path := codec__field_at(k, "at", Self, codec__path(k), ONCE)
	scope := codec__field_at(k, "scope", Self, codec__page_filter__all(k), UNIQUE)
	favicon := codec__flag_at(k, "favicon", Self)

  inner_loop := codec__loop(k, codec__sum(k, id, path, favicon, scope))
	return codec__remote_push(k, "icons", Page, codec__loc(k, inner_loop))
}
// }}}
// {{{ Link definitions
Def__Link :: struct {
	id:     string,
	target: string, // url
	label:  Inline_Markup,
  scope:  Page_Filter__Scope, // Link definitions can affect other pages
  loc:    Source_Loc,
}

@(private = "file")
codec__deflink :: proc(k: ^Codec_Kit) -> ^Codec {
  Self :: Def__Link
  ctext := codec__contiguous_text(k)
  imarkup := codec__inline_markup(k)

	id     := codec__field(k, "id", Self, ctext, REQUIRED)
  target := codec__field_at(k, "target", Self, codec__raw(k), ONCE)
  label  := codec__field_at(k, "label", Self, imarkup, UNIQUE)
  scope  := codec__field_at(k, "scope", Self, codec__page_filter__all(k), UNIQUE)
  loop   := codec__loop(k, codec__sum(k, label, target, scope, id))
	return codec__remote_push(k, "links", Page, codec__loc(k, loop))
}
// }}}
// {{{ Footnote definitions
Def__Footnote :: struct {
	id:      string,
	content: Block_Markup,
  index:   uint, // The page-local number used to display the footnote
  loc:     Source_Loc,
}

@(private = "file")
codec__defnote :: proc(k: ^Codec_Kit) -> ^Codec {
  ctext := codec__contiguous_text(k)
  bmarkup := codec__block_markup(k)

	id      := codec__field_at(k, "id", Def__Footnote, ctext, ONCE)
  content := codec__field(k, "content", Def__Footnote, bmarkup, REQUIRED)
  loop    := codec__loop(k, codec__sum(k, content, id))
	return codec__remote_push(k, "footnotes", Page, codec__loc(k, loop))
}
// }}}
// {{{ Feed definitions
Def__Feed :: struct {
  at:          Path,
  name:        string,
  description: string,

  members: Page_Filter__All, // What posts should this include?
  under:   Page_Filter__All, // Which pages should this appear on?
  aliases: Exparr(Path__Output), // Locations to redirect from

  // Generated
  site_path: Path__Output,
}

@(private = "file")
codec__feed :: proc(k: ^Codec_Kit) -> ^Codec {
  Self :: Def__Feed

  text := codec__text(k)
  filter := codec__page_filter__all(k)

	at := codec__field(k, "at", Self, codec__path(k), REQUIRED)
	name := codec__field_at(k, "name", Self, text, REQUIRED)
	desc := codec__field_at(k, "description", Self, text, REQUIRED)
	under := codec__field_at(k, "under", Self, filter, ONCE, UNIQUE)
	members := codec__field_at(k, "members", Self, filter, ONCE)

  aliases_payload := codec__exparr(k, codec__at(k, "alias", codec__out_path(k)))
  aliases := codec__field(k, "aliases", Self, aliases_payload)

  all := codec__sum(k, at, under, members, aliases, name, desc)
  return codec__loop(k, all)
}
// }}}
// {{{ Stylesheet definitions
Def__Stylesheet :: struct {
  scope:   Page_Filter__All,
  preload: bool,
  at:      Path,

  // Generated
  site_path: Path__Output,
}

@(private = "file")
codec__stylesheet :: proc(k: ^Codec_Kit) -> ^Codec {
  Self :: Def__Stylesheet
	scope := codec__field(k, "scope", Self, codec__page_filter__all(k))
	path := codec__field_at(k, "at", Self, codec__path(k), ONCE)
  preload := codec__flag_at(k, "preload", Self)
  return codec__loop(k, codec__sum(k, scope, path, preload))
}
// }}}
// {{{ Assets
Def__Asset :: struct {
  from: Path,
  to:   Path,
}

@(private = "file")
codec__asset :: proc(k: ^Codec_Kit) -> ^Codec {
  Self :: Def__Asset
  path := codec__path(k)
	from := codec__field(k, "from", Self, path, ONCE)
	to   := codec__field_at(k, "to", Self, path, UNIQUE)
  return codec__loop(k, codec__sum(k, from, to))
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
codec__heading :: proc(k: ^Codec_Kit, level: uint) -> ^Codec {
  // This would benefit from having a "tap"-style helper, but oh well.
  lens :: proc(kit: ^Lens_Kit) {
    outer := cast(^Heading)kit.outer
    inner := cast(^Heading)kit.inner

    switch kit.mode {
    case .Project: inner^ = outer^
    case .Inject:
      outer^ = inner^
      outer.level = mem.reinterpret_copy(uint, kit.user_data)
      log.assert(0 < outer.level)
      log.assert(outer.level <= MAX_HEADING_LEVEL)
    }
  }

  ctext := codec__contiguous_text(k)
  imarkup := codec__inline_markup(k)

	id := codec__field_at(k, "id", Heading, ctext, UNIQUE)
  content := codec__field(k, "content", Heading, imarkup, REQUIRED)
  looped := codec__loop(k, codec__sum(k, content, id))
  with_level := codec__focus(k, Heading, looped, lens, level)
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
codec__table :: proc(k: ^Codec_Kit) -> ^Codec {
  imarkup := codec__inline_markup(k)
	cell_payload := codec__field(k, "content", Table__Cell, imarkup)
	cell := codec__at(k, "cell", cell_payload)
	row := codec__field(k, "cells", Table__Row, codec__spaced_exparr(k, cell))

	caption := codec__field(k, "caption", Table, imarkup)
	header := codec__field_at(k, "header", Table, row, ONCE)

  rows_payload := codec__exparr(k, codec__at(k, "row", row))
	rows := codec__field(k, "rows", Table, rows_payload)

	return codec__loop(k, codec__sum(k, caption, header, rows))
}

table__check :: proc(site: ^Site, page: ^Page, table: ^Table) {
  inline_markup__check(site, page, &table.caption)
  table__row__check(site, page, &table.header)
  for iter := iter__mk(table.rows); row in iter__next(&iter) {
    table__row__check(site, page, row)
  }
}

table__row__check :: proc(site: ^Site, page: ^Page, row: ^Table__Row) {
  for iter := iter__mk(row.cells); cell in iter__next(&iter) {
    mem__non_zero(cell.content.elements) or_continue
    inline_markup__check(site, page, &cell.content)
  }
}
// }}}
// {{{ Timestamps
@(private = "file")
codec__timestamp :: proc(k: ^Codec_Kit) -> ^Codec {
  lens :: proc(kit: ^Lens_Kit) {
    outer := cast(^time.Time)kit.outer
    inner := cast(^string)kit.inner
    switch kit.mode {
    case .Project:
      log.assertf(outer^ == {}, "Timestamps must parse in one go: %v", outer^)
    case .Inject:
      if inner^ == "" {
        kit.ignored = true
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

      lens__errorf(kit, "Invalid timestamp: '%v'", inner^)
    }
  }

  return codec__tracked(
    k,
    codec__focus(k, time.Time, codec__contiguous_text(k), lens, scratch = true),
    "timestamp",
    UNIQUE
  )
}
// }}}
// {{{ Integers
// NOTE: this does not currently handle signed integers (and *will* overflow for
// large enough integers).
@(private = "file")
codec__integer :: proc(k: ^Codec_Kit, $T: typeid) -> ^Codec {
  lens :: proc(kit: ^Lens_Kit) {
    outer := cast(^T)kit.outer
    inner := cast(^string)kit.inner
    switch kit.mode {
    case .Project:
      log.assertf(outer^ == {}, "Integers must parse in one go: %v", outer^)
    case .Inject:
      if inner^ == "" {
        kit.ignored = true
        return
      }

      n: int
      value, ok := strconv.parse_u64_maybe_prefixed(inner^, &n)
      if !ok || n != len(inner) {
        lens__errorf(kit, "Invalid integer: '%v'", inner)
      } else if u64(T(value)) != value {
        lens__errorf(kit, "Integer would overflow: '%v'", value)
      } else {
        outer^ = T(value)
        return
      }
    }
  }

  return codec__tracked(
    k,
    codec__focus(k, T, codec__contiguous_text(k), lens, scratch = true),
    "integer",
    UNIQUE,
  )
}
// }}}
// {{{ Text
// A sequence of text where all the whitespace in the source is discarded
@(private = "file")
codec__contiguous_text :: proc(k: ^Codec_Kit) -> ^Codec {
	return codec__memo(
		k, "contiguous_text", string,
		proc(k: ^Codec_Kit) -> ^Codec {
			return codec__focus(
				k,
				string,
				codec__spaced_exparr(k, codec__string(k)),
        codec__text__lens,
        scratch = true
			)
		},
	)
}

@(private = "file")
codec__text :: proc(k: ^Codec_Kit) -> ^Codec {
	return codec__memo(
		k, "text", string,
		proc(k: ^Codec_Kit) -> ^Codec {
      str   := codec__string(k)
      space := codec__space(k, " ")
      inner := codec__exparr(k, codec__sum(k, str, space))

			return codec__focus(
				k,
				string,
				inner,
        codec__text__lens,
        scratch = true
			)
		},
	)
}

@(private = "file")
codec__text__lens :: proc(kit: ^Lens_Kit) {
  inner := cast(^Exparr(string))kit.inner
  outer := cast(^string)kit.outer

  switch kit.mode {
  case .Project:
    inner.allocator = kit.temp_allocator
    if outer^ != "" do push(inner, outer^)
  case .Inject:
    size: uint = 0
    for iter := iter__mk(inner^); x in iter__next(&iter) do size += len(x)

    builder := strings__fixed_builder(size, kit.allocator)
    for iter := iter__mk(inner^); chunk in iter__next(&iter) {
      size := len(builder.buf)
      if chunk^ == " " && size > 0 && builder.buf[size - 1] == ' ' do continue
      strings.write_string(&builder, chunk^)
    }

    outer^ = strings.to_string(builder)
  }
}
// }}}
// {{{ Paths
@(private = "file")
codec__path :: proc(k: ^Codec_Kit) -> ^Codec {
  return codec__transmute(k, Path, codec__contiguous_text(k))
}

@(private = "file")
codec__out_path :: proc(k: ^Codec_Kit) -> ^Codec {
  return codec__transmute(k, Path__Output, codec__contiguous_text(k))
}
// }}}
// {{{ Article lists
Article_List :: struct {
  filter:  Page_Filter__All,
  heading: u8,
  loc:     Source_Loc,
}

@(private = "file")
codec__article_list :: proc(k: ^Codec_Kit) -> ^Codec {
  filter__all := codec__page_filter__all(k)
  u8 := codec__integer(k, u8)

	filter := codec__field(k, "filter", Article_List, filter__all)
  heading := codec__field_at(k, "heading", Article_List, u8, UNIQUE)
  looped := codec__loop(k, codec__sum(k, filter, heading))
	return codec__loc(k, looped)
}
// }}}
// {{{ Tags
Tag :: distinct string

@(private = "file")
codec__tag :: proc(k: ^Codec_Kit) -> ^Codec {
  return codec__transmute(k, Tag, codec__contiguous_text(k))
}
// }}}
// {{{ Redirects
Redirect :: struct {
  from: Path__Output,
  to:   Path__Output,
}
// }}}

// {{{ Inline markup
Inline_Markup__Space :: distinct Unit
Inline_Markup__Ellipsis :: distinct Unit
Inline_Markup__Text :: distinct string
Inline_Markup__Emph :: distinct Inline_Markup
Inline_Markup__Strong :: distinct Inline_Markup
Inline_Markup__Strikethrough :: distinct Inline_Markup
Inline_Markup__Mono :: distinct string
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
) -> ^Codec {
  Self :: Inline_Markup__Timestamp

  time := codec__field(k, "time", Self, codec__timestamp(k), ONCE)
  compact := codec__flag_at(k, "compact", Self)

  return codec__loop(k, codec__sum(k, time, compact))
}

@(private = "file")
codec__inline_markup__atom :: proc(
  k: ^Codec_Kit
) -> ^Codec {
	imarkup := codec__inline_markup(k)
	ctext := codec__contiguous_text(k)

	space := codec__space(k, Inline_Markup__Space{})
	text := codec__transmute(k, Inline_Markup__Text, codec__string(k))
	ellipsis__sugar := codec__token(k, .Ellipsis, Inline_Markup__Ellipsis{})
	ellipsis__basic := codec__const(k, "...", Inline_Markup__Ellipsis{})

  emph := codec__transmute(k, Inline_Markup__Emph, imarkup)
	emph__sugar := codec__delim(k, .Underscore, .Underscore, emph)
	emph__basic := codec__at(k, "emph", emph)

  strong := codec__transmute(k, Inline_Markup__Strong, imarkup)
	strong__sugar := codec__delim(k, .Asterisk, .Asterisk, strong)
	strong__basic := codec__at(k, "strong", strong)

  strike := codec__transmute(k, Inline_Markup__Strikethrough, imarkup)
	strike__sugar := codec__delim(k, .Tilde, .Tilde, strike)
	strike__basic := codec__at(k, "strike", strike)

  mono := codec__transmute(k, Inline_Markup__Mono, codec__raw(k))
	mono__sugar := codec__delim(k, .Backtick, .Backtick, mono)
	mono__basic := codec__at(k, "mono", mono)

  quote := codec__transmute(k, Inline_Markup__Quote, imarkup)
	quote__sugar := codec__delim(k, .Quote, .Quote, quote)
	quote__basic := codec__at(k, "quote", quote)

  icon_id := codec__field(k, "id", Inline_Markup__Icon, ctext, ONCE)
  icon := codec__ref(k, codec__at(k, "icon", codec__loc(k, icon_id)))

  fn_id := codec__field(k, "id", Inline_Markup__Fn, ctext, ONCE)
  fn := codec__ref(k, codec__at(k, "fn", codec__loc(k, fn_id)))

  Link :: Inline_Markup__Link
  link_id := codec__field(k, "id", Link, ctext, ONCE)
  link_label__basic := codec__at(k, "label", imarkup)
  link_label__sugar := codec__leaded(k, .Bar, imarkup)
  link_label__payload := codec__sum(k, link_label__basic, link_label__sugar)
  link_label := codec__field(k, "label", Link, link_label__payload, UNIQUE)
  link_sum := codec__sum(k, link_label, link_id)
  link_payload := codec__loc(k, codec__loop(k, link_sum))
  link__sugar := codec__ref(k, codec__at(k, "link", link_payload))
  link__basic := codec__ref(k, codec__delim(k, .LSquare, .RSquare, link_payload))

  timestamp := codec__inline_markup__timestamp(k)
  date := codec__trans_at(k, "date", Inline_Markup__Date, timestamp)
  datetime := codec__trans_at(k, "datetime", Inline_Markup__Datetime, timestamp)

	return codec__union(
		k,
    Inline_Markup__Atom,
    { Inline_Markup__Space,         space          },
    { Inline_Markup__Text,          text           },
    { Inline_Markup__Ellipsis,      ellipsis__sugar },
    { Inline_Markup__Ellipsis,      ellipsis__basic },
    { Inline_Markup__Emph,          emph__sugar     },
    { Inline_Markup__Emph,          emph__basic     },
    { Inline_Markup__Strong,        strong__sugar   },
    { Inline_Markup__Strong,        strong__basic   },
    { Inline_Markup__Strikethrough, strike__sugar   },
    { Inline_Markup__Strikethrough, strike__basic   },
    { Inline_Markup__Mono,          mono__sugar     },
    { Inline_Markup__Mono,          mono__basic     },
    { Inline_Markup__Quote,         quote__sugar    },
    { Inline_Markup__Quote,         quote__basic    },
    { Inline_Markup__Date,          date           },
    { Inline_Markup__Datetime,      datetime       },
    { ^Inline_Markup__Icon,         icon           },
    { ^Inline_Markup__Fn,           fn             },
    { ^Inline_Markup__Link,         link__sugar     },
    { ^Inline_Markup__Link,         link__basic     },
	)
}

@(private="file")
codec__inline_markup :: proc(kit: ^Codec_Kit) -> ^Codec {
	return codec__memo(
		kit,
		"inline_markup",
    Inline_Markup,
		proc(k: ^Codec_Kit) -> ^Codec {
      // This lens marks runs only consisting of spaces as ignored.
      lens :: proc(kit: ^Lens_Kit) {
        switch kit.mode {
        case .Project:
          mem.copy(kit.inner, kit.outer, size_of(Inline_Markup))
        case .Inject:
          inner := cast(^^Exparr(Inline_Markup__Atom))kit.inner

          found_substantial := false
          for iter := iter__mk(inner^^); elem in iter__next(&iter) {
            if _, ok := elem.(Inline_Markup__Space); !ok {
              found_substantial = true
              break
            }
          }

          if found_substantial {
            mem.copy(kit.outer, kit.inner, size_of(Inline_Markup))
          } else {
            kit.ignored = true
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
@(private="file")
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
    fmt.wprintf(fi.writer, "`%v`", string(inner))
  case Inline_Markup__Quote:
    fmt.wprint(fi.writer, QUOTE_EN_LEFT)
    inline_markup__fmt(fi, site, page, Inline_Markup(inner))
    fmt.wprint(fi.writer, QUOTE_EN_RIGHT)
  case ^Inline_Markup__Link:
    if mem__non_zero(inner.label) {
      inline_markup__fmt(fi, site, page, inner.label)
    } else if inner.def != nil && mem__non_zero(inner.def.label) {
      inline_markup__fmt(fi, site, page, inner.def.label)
    } else if inner.def != nil {
      fmt.wprintf(fi.writer, inner.def.id)
    } else {
      fmt.wprint(fi.writer, inner.id)
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

@(private="file")
inline_markup__fmt :: proc(
  fi: ^fmt.Info, site: Site, page: Page, im: Inline_Markup
) {
  if im.elements == nil do return
  for iter := iter__mk(im.elements^); chunk in iter__next(&iter) {
    inline_markup__atom__fmt(fi, site, page, chunk^)
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
@(private="file")
inline_markup__atom__html :: proc(
  g: ^Xml_Gen, page: Page, atom: Inline_Markup__Atom
) {
  switch inner in atom {
  case nil:
  case ^Inline_Markup__Icon:
    xml__attr(g, "class", "icon")
    // Decorative image.
    // See: https://www.w3.org/WAI/tutorials/images/decorative/
    xml__attr(g, "alt", "")
    xml__attr(g, "src", site__url(g.site, inner.def.site_path, .Stack))
  case Inline_Markup__Space:
    xml__string(g, " ")
  case Inline_Markup__Ellipsis:
    xml__stringf(g, "%v", ELLIPSIS_SYMBOL)
  case Inline_Markup__Text:
    xml__string(g, string(inner))
  case Inline_Markup__Emph:
    xml__tag(g, "em")
    inline_markup__html(g, page, Inline_Markup(inner))
  case Inline_Markup__Strong:
    xml__tag(g, "strong")
    inline_markup__html(g, page, Inline_Markup(inner))
  case Inline_Markup__Strikethrough:
    xml__tag(g, "s")
    inline_markup__html(g, page, Inline_Markup(inner))
  case Inline_Markup__Mono:
    xml__tag(g, "code")
    xml__string(g, string(inner))
  case Inline_Markup__Quote:
    xml__stringf(g, "%v", QUOTE_EN_LEFT)
    inline_markup__html(g, page, Inline_Markup(inner))
    xml__stringf(g, "%v", QUOTE_EN_RIGHT)
  case ^Inline_Markup__Link:
    xml__tag(g, "a")
    xml__attr(g, "href", inner.def.target)
    if mem__non_zero(inner.label) {
      inline_markup__html(g, page, inner.label)
    } else if inner.def != nil && mem__non_zero(inner.def.label) {
      inline_markup__html(g, page, inner.def.label)
    } else if inner.def != nil {
      xml__string(g, inner.def.id)
    } else {
      xml__string(g, inner.id)
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
  g: ^Xml_Gen, page: Page, im: Inline_Markup
) {
  if im.elements == nil do return
  for iter := iter__mk(im.elements^); chunk in iter__next(&iter) {
    inline_markup__atom__html(g, page, chunk^)
  }
}
// }}}
// {{{ Checking
@(private="file")
inline_markup__check :: proc(site: ^Site, page: ^Page, im: ^Inline_Markup) {
  if im.elements == nil do return

  // Remove spurious leading/trailing spaces.
  for _ in 0..<2 {
    exparr__reverse(im.elements^)
    for chunk in exparr__try_last(im.elements^) {
      _ = chunk.(Inline_Markup__Space) or_break
      exparr__pop(im.elements)
    }
  }

  for iter := iter__mk(im.elements^); chunk in iter__next(&iter) {
    inline_markup__atom__check(site, page, chunk)
  }
}

@(private="file")
ambiguous_reference_error :: proc(
  site: ^Site,
  id: string,                  // The referenced ID we couldn't resolve
  loc: Source_Loc,             // Where did the reference occurr
  options: Exparr(Source_Loc), // The possible things the ID could've resolve to
) {
  log.assert(options.len > 1)
  options := options // We refer to this by pointer below.

  site__errorf(
    site,
    loc,
    "Ambiguous reference to '%v': cannot decide between %v.",
    id,
    fmt__freeze1(&options, proc(fi: ^fmt.Info, options: ^Exparr(Source_Loc)) {
      for iter := iter__mk(options^); option, i in iter__next(&iter) {
        if i > 0 do fmt.wprint(fi.writer, ", ")
        if i == options.len - 1 do fmt.wprint(fi.writer, "and ")
        fmt.wprint(fi.writer, option^)
      }
    })
  )
}

@(private="file")
inline_markup__atom__check :: proc(
  site: ^Site, page: ^Page, atom: ^Inline_Markup__Atom
) {
  switch &inner in atom {
  case nil:
  case Inline_Markup__Space:
  case Inline_Markup__Ellipsis:
  case Inline_Markup__Date: 
  case Inline_Markup__Datetime: 
  case Inline_Markup__Mono:
  case Inline_Markup__Text:
    for char in string(inner) {
      unicode.is_alpha(char) or_continue
      page.word_count += 1
      break
    }
  case Inline_Markup__Emph:
    inline_markup__check(site, page, cast(^Inline_Markup)&inner)
  case Inline_Markup__Strong:
    inline_markup__check(site, page, cast(^Inline_Markup)&inner)
  case Inline_Markup__Strikethrough:
    inline_markup__check(site, page, cast(^Inline_Markup)&inner)
  case Inline_Markup__Quote:
    inline_markup__check(site, page, cast(^Inline_Markup)&inner)
  case ^Inline_Markup__Link:
    log.assert(inner.def == nil)

    if mem__non_zero(inner.label) {
      inline_markup__check(site, page, &inner.label)
    }

    site__frame(site)
    options := Exparr(Source_Loc) { allocator = site__alloc(site, .Stack) }

    for iter := iter__mk(site.pages); defsite in iter__next(&iter) {
      for iter := iter__mk(defsite.links); link in iter__next(&iter) {
        (link.id == inner.id) or_continue
        page_filter__scope__eval(defsite^, page^, link.scope) or_continue
        push(&options, link.loc)
        inner.def = link
      }
    }

    if inner.def == nil {
      site__errorf(site, inner.loc, "Link '%v' is not in scope.", inner.id)
    } else if options.len > 1 {
      ambiguous_reference_error(site, inner.id, inner.loc, options)
    }
  case ^Inline_Markup__Fn:
    log.assert(inner.def == nil)

    site__frame(site)
    options := Exparr(Source_Loc) { allocator = site__alloc(site, .Stack) }

    for iter := iter__mk(page.footnotes); footnote in iter__next(&iter) {
      (footnote.id == inner.id) or_continue
      push(&options, footnote.loc)
      inner.def = footnote
    }

    if inner.def == nil {
      site__errorf(site, inner.loc, "Footnote '%v' is not in scope.", inner.id)
    } else if options.len > 1 {
      ambiguous_reference_error(site, inner.id, inner.loc, options)
    }

  case ^Inline_Markup__Icon:
    log.assert(inner.def == nil)

    site__frame(site)
    options := Exparr(Source_Loc) { allocator = site__alloc(site, .Stack) }

    for iter := iter__mk(site.pages); defsite in iter__next(&iter) {
      for iter := iter__mk(defsite.icons); icon in iter__next(&iter) {
        (icon.id == inner.id) or_continue
        page_filter__scope__eval(defsite^, page^, icon.scope) or_continue
        push(&options, icon.loc)
        inner.def = icon
      }
    }

    if inner.def == nil {
      site__errorf(site, inner.loc, "Icon '%v' is not in scope.", inner.id)
    } else if options.len > 1 {
      ambiguous_reference_error(site, inner.id, inner.loc, options)
    }
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

// These are inserted after the fact, during the checking phase
Block_Markup__Section :: struct {
  heading: ^Heading,
  content: Block_Markup,
}

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
  Block_Markup__Aside,
  Block_Markup__Code,
  Block_Markup__Section,
  Article_List,
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
) -> ^Codec {
  ctext := codec__contiguous_text(k)
	imarkup := codec__inline_markup(k)
	source_ref := codec__field_at(k, "source", Block_Markup__Image, ctext, ONCE)
	alt_ref := codec__field(k, "alt", Block_Markup__Image, imarkup, ONCE)
	return codec__loop(k, codec__sum(k, source_ref, alt_ref))
}

@(private = "file")
codec__block_markup__figure :: proc(
  k: ^Codec_Kit
) -> ^Codec {
  imarkup := codec__inline_markup(k)
  bmarkup := codec__block_markup(k)
	caption := codec__field_at(k, "caption", Block_Markup__Figure, imarkup, UNIQUE)
	content := codec__field(k, "content", Block_Markup__Figure, bmarkup, ONCE)
	return codec__loop(k, codec__sum(k, caption, content))
}

@(private = "file")
codec__block_markup__aside :: proc(
  k: ^Codec_Kit
) -> ^Codec {
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
    codec__sum(k, content, id, icon, title, collapse, content)
  )
}

// This one is implemented in a very silly way, with the benefit being that
// there's no actual branching support required in the proper codec system. We
// instead hack our own by simply trying both options and using the "ignored"
// field of the kit to filter out the invalid ones.
codec__block_markup__list :: proc(
  k: ^Codec_Kit
) -> ^Codec {
  Self :: Block_Markup__List

  ilens :: proc(kit: ^Lens_Kit) {
    outer := cast(^Self)kit.outer
    inner := cast(^Exparr(Inline_Markup))kit.inner
    switch kit.mode {
    case .Project:
      if outer.block do kit.ignored = true
      else do inner^ = outer.imarkup
    case .Inject:
      log.assert(!outer.block)
      outer.imarkup = inner^
    }
  }

  blens :: proc(kit: ^Lens_Kit) {
    outer := cast(^Self)kit.outer
    inner := cast(^Exparr(Block_Markup))kit.inner
    switch kit.mode {
    case .Project:
      if !outer.block do kit.ignored = true
      else do inner^ = outer.bmarkup
    case .Inject:
      log.assert(outer.block)
      outer.bmarkup = inner^
    }
  }

  ordered := codec__flag_at(k, "ordered", Self)
  block := codec__flag_at(k, "block", Self)
  flags := codec__sum(k, ordered, block)

  imarkup := codec__inline_markup(k)
  ielem__sugar := codec__at(k, "item", imarkup)
  ielem__basic := codec__leaded(k, .Asterisk, imarkup)
  ielem := codec__sum(k, ielem__sugar, ielem__basic)
  icontent := codec__focus(k, Self, codec__spaced_exparr(k, ielem), ilens)

  bmarkup := codec__block_markup(k)
  belem__sugar := codec__at(k, "item", bmarkup)
  belem__basic := codec__leaded(k, .Asterisk, bmarkup)
  belem := codec__sum(k, belem__sugar, belem__basic)
  bcontent := codec__focus(k, Self, codec__spaced_exparr(k, belem), blens)

  content := codec__sum(k, icontent, bcontent)
	return codec__loop(k, codec__seq(k, flags, content))
}

codec__block_markup__code :: proc(
  k: ^Codec_Kit
) -> ^Codec {
  Self :: Block_Markup__Code

  ctext := codec__contiguous_text(k)
  lang := codec__field(k, "language", Self, ctext, ONCE)
  content := codec__field_at(k, "content", Self, codec__raw(k), ONCE)

	return codec__loop(k, codec__sum(k, lang, content))
}

@(private = "file")
codec__block_markup__atom :: proc(
  k: ^Codec_Kit
) -> ^Codec {
	imarkup := codec__inline_markup(k)
	bmarkup := codec__block_markup(k)

	description := codec__const(k, "embed-description", Block_Markup__Description{})
	thematic_break := codec__const(k, "---", Block_Markup__Thematic_Break{})
	table_of_contents := codec__const(k, "toc", Block_Markup__Table_Of_Contents{})

	blockquote := codec__transmute(k, Block_Markup__Blockquote, bmarkup)
  blockquote__sugar := codec__leaded(k, .GT, blockquote)
  blockquote__basic := codec__at(k, ">", blockquote)

	image := codec__at(k, "image", codec__block_markup__image(k))
	figure := codec__at(k, "figure", codec__block_markup__figure(k))
	para := codec__transmute(k, Block_Markup__Paragraph, codec__para(k, imarkup))
	table := codec__at(k, "table", codec__table(k))
	deflink := codec__at(k, "deflink", codec__deflink(k))
	defnote := codec__at(k, "defnote", codec__defnote(k))
  deficon := codec__at(k, "deficon", codec__deficon(k))
	aside := codec__at(k, "aside", codec__block_markup__aside(k))
  article_list := codec__at(k, "index", codec__article_list(k))
  list := codec__at(k, "list", codec__block_markup__list(k))
  code := codec__at(k, "code", codec__block_markup__code(k))

  h2 := codec__heading(k, 2)
  h3 := codec__heading(k, 3)
  h4 := codec__heading(k, 4)
	h2__basic := codec__at(k, "h1", h2)
	h3__basic := codec__at(k, "h2", h3)
	h4__basic := codec__at(k, "h3", h4)
	h2__sugar := codec__leaded(k, .Hash, h2)
	h3__sugar := codec__leaded(k, .Double_Hash, h3)
	h4__sugar := codec__leaded(k, .Triple_Hash, h4)

  return codec__union(
		k,
    Block_Markup__Atom,
    { Block_Markup__Blockquote,        blockquote__sugar  },
    { Block_Markup__Blockquote,        blockquote__basic  },
    { Block_Markup__Description,       description       },
    { Block_Markup__Table_Of_Contents, table_of_contents },
    { Block_Markup__Thematic_Break,    thematic_break    },
    { Block_Markup__Image,             image             },
    { Block_Markup__Figure,            figure            },
    { Table,                          table             },
    { ^Heading,                       h2__sugar          },
    { ^Heading,                       h3__sugar          },
    { ^Heading,                       h4__sugar          },
    { ^Heading,                       h2__basic          },
    { ^Heading,                       h3__basic          },
    { ^Heading,                       h4__basic          },
    { Article_List,                   article_list      },
    { Block_Markup__List,              list              },
    { Block_Markup__Code,              code              },
    { Block_Markup__Aside,             aside             },
    { ^Def__Link,                      deflink           },
    { ^Def__Footnote,                  defnote           },
    { ^Def__Icon,                      deficon           },
    { Block_Markup__Paragraph,         para              },
	)
}

@(private="file")
codec__block_markup :: proc(kit: ^Codec_Kit) -> ^Codec {
	return codec__memo(
		kit,
		"block_markup",
    Block_Markup,
		proc(kit: ^Codec_Kit) -> ^Codec {
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
@(rodata)
@(private="file")
HEADING_TAG_NAMES: [MAX_HEADING_LEVEL]string = {"h1", "h2", "h3", "h4"}

@(private="file")
block_markup__anchored_heading :: proc(
  g: ^Xml_Gen, page: Page, heading: Heading, main := false
) {
  xml__tag(g, HEADING_TAG_NAMES[heading.level - 1])
  xml__attr(g, "id", heading.id)

  if xml__tag(g, "a") {
    xml__attr(g, "class", "heading-anchor")

    if !main && mem__non_zero(heading.id) {
      xml__attrf(g, "href", "#%v", heading.id)
    } else {
      xml__attr(g, "href", "")
    }

    xml__string(g, "◇")
  }

  xml__string(g, " ")
  inline_markup__html(g, page, heading.content)
}

@(private="file")
block_markup__atom__html :: proc(
  g: ^Xml_Gen, page: Page, atom: Block_Markup__Atom
) {
  switch inner in atom {
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
    inline_markup__html(g, page, page.description)
  case Block_Markup__Table_Of_Contents:
    xml__tag(g, "details")
    if xml__tag(g, "summary") do xml__string(g, "Toggle table of contens")
    xml__tag(g, "nav")
    xml__attr(g, "role", "doc-toc")
    xml__attr(g, "aria-labelledby", "toc-title")
    if xml__tag(g, "h3") {
      xml__attr(g, "id", "toc-title")
      xml__string(g, "Table of Contents")
    }

    xml__tag(g, "ol")
    stack: [dynamic; MAX_HEADING_LEVEL]uint
    // Whether we've created an <ol> element for the top of the stack. Since an
    // empty stack is contained in the <ol> we've just created above, this
    // starts out as being true.
    last_has_children := true
    for iter := iter__mk(page.headings); heading in iter__next(&iter) {
      #reverse for last in stack {
        (last >= heading.level) or_break
        pop(&stack)
        if last_has_children do xml__tag_end(g, "ol")
        xml__tag_end(g, "li")
        last_has_children = true
      }

      if !last_has_children {
        xml__tag(g, "ol", auto_close = false)
      }

      push(&stack, heading.level)
      xml__tag(g, "li", auto_close = false)
      xml__tag(g, "a")
      xml__attrf(g, "href", "#%v", heading.id)
      inline_markup__html(g, page, heading.content)
      last_has_children = false
    }

    // Clean what's left of the stack
    for len(stack) > 0 {
      pop(&stack)
      if last_has_children do xml__tag_end(g, "ol")
      xml__tag_end(g, "li")
      last_has_children = true
    }
  case Article_List:
    xml__tag(g, "ol")
    xml__attr(g, "class", "article-list")
    for iter := iter__mk(g.site.pages); article in iter__next(&iter) {
      page_filter__eval(page, article^, inner.filter) or_continue

      xml__tag(g, "li")
      xml__tag(g, "article")

      if xml__tag(g, HEADING_TAG_NAMES[inner.heading - 1]) {
        xml__tag(g, "a")
        xml__attrf(g, "href", "%v", article.url)
        xml__attr(g, "rel", "bookmark")
        inline_markup__html(g, article^, article.title)
      }

      if xml__tag(g, "ul") {
        if xml__tag(g, "li") {
          xml__stringf(g, "%v by ", fmt__posted_on(&article.published_at))
          xml__tag(g, "a")
          xml__attrf(g, "href", "%v", g.site.base_url)
          xml__attr(g, "rel", "bookmark")
          xml__string(g, USERNAME)
        }

        if xml__tag(g, "li") {
          at := page__last_updated(article^)
          xml__stringf(g, "Last updated on %v", Datetime__Pretty(at))
        }

        if xml__tag(g, "li") {
          at := page__last_updated(article^)
          xml__stringf(
            g, "About %v words; a %v read",
            fmt__word_count(&article.word_count),
            fmt__reading_duration(&article.word_count),
          )
        }
      }

      xml__tag(g, "p")
      inline_markup__html(g, article^, article.description)
    }
  case Block_Markup__Section:
    xml__tag(g, "section")
    xml__attr(g, "aria-labelledby", inner.heading.id)
    block_markup__anchored_heading(g, page, inner.heading^)
    block_markup__html(g, page, inner.content)
  case Block_Markup__Paragraph:
    xml__tag(g, "p")
    inline_markup__html(g, page, Inline_Markup(inner))
  case Block_Markup__Blockquote:
    xml__tag(g, "blockquote")
    block_markup__html(g, page, Block_Markup(inner))
  case Block_Markup__List:
    xml__tag(g, inner.ordered ? "ol" : "ul")
    if inner.block {
      for iter := iter__mk(inner.bmarkup); elem in iter__next(&iter) {
        xml__tag(g, "li")
        block_markup__html(g, page, elem^)
      }
    } else {
      for iter := iter__mk(inner.imarkup); elem in iter__next(&iter) {
        xml__tag(g, "li")
        inline_markup__html(g, page, elem^)
      }
    }
  case Block_Markup__Code:
    xml__tag(g, "pre")
    xml__tag(g, "code")
    xml__attr(g, "data-language", inner.language)
    xml__string(g, inner.content)
  case Block_Markup__Aside:  // TODO
  case Block_Markup__Image:  // TODO
  case Block_Markup__Figure: // TODO
  case Table:
    if mem__non_zero(inner.caption) {
      xml__tag(g, "caption")
      inline_markup__html(g, page, inner.caption)
    }

    row__html :: proc(g: ^Xml_Gen, page: Page, row: Table__Row, kind: string) {
      xml__tag(g, "tr")
      for iter := iter__mk(row.cells); cell in iter__next(&iter) {
        xml__tag(g, kind)
        inline_markup__html(g, page, cell.content)
      }
    }

    row__html(g, page, inner.header, "th")
    for iter := iter__mk(inner.rows); row in iter__next(&iter) {
      row__html(g, page, row^, "td")
    }
  }
}

block_markup__html :: proc(
  g: ^Xml_Gen, page: Page, bm: Block_Markup
) {
  for iter := iter__mk(bm.elements); chunk in iter__next(&iter) {
    block_markup__atom__html(g, page, chunk^)
  }
}
// }}}
// {{{ Checking
@(private="file")
block_markup__check :: proc(site: ^Site, page: ^Page, bm: ^Block_Markup) {
  if bm == nil do return
  for iter := iter__mk(bm.elements); atom in iter__next(&iter) {
    block_markup__atom__check(site, page, atom)
  }

  // Split block into sections We don't attempt to reuse any of the existing
  // structure... As such, we try to avoind neednessly creating new copies when
  // the body contains no headings.
  has_headings: bool
  for iter := iter__mk(bm.elements); atom in iter__next(&iter) {
    heading := atom.(^Heading) or_continue
    has_headings = true
    break
  }
  if !has_headings do return

  sectioned: Block_Markup // The root section we write to
  sectioned.elements.allocator = site__alloc(site, .Forever)

  stack: [dynamic; MAX_HEADING_LEVEL]^Block_Markup__Section
  for iter := iter__mk(bm.elements); atom in iter__next(&iter) {
    if heading, ok := atom.(^Heading); ok {
      #reverse for last in stack {
        (last.heading.level >= heading.level) or_break
        pop(&stack)
      }

      inner_content: Block_Markup
      inner_content.elements.allocator = site__alloc(site, .Forever)
      atom := Block_Markup__Section {
        heading = heading,
        content = inner_content,
      }

      ref: ^Block_Markup__Atom
      if l := len(stack); l > 0 {
        last := stack[l - 1]
        ref = push(&last.content.elements, atom)
      } else {
        ref = push(&sectioned.elements, atom)
      }

      section := &ref.(Block_Markup__Section)
      push(&stack, section)
    } else if l := len(stack); l > 0 {
      last := stack[l - 1]
      push(&last.content.elements, atom^)
    } else {
      push(&sectioned.elements, atom^)
    }
  }

  bm^ = sectioned
}

@(private="file")
block_markup__atom__check :: proc(
  site: ^Site, page: ^Page, atom: ^Block_Markup__Atom
) {
  switch &inner in atom {
  case nil:
  case Block_Markup__Code:
  case Block_Markup__Description:
  case Block_Markup__Table_Of_Contents:
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
      for iter := iter__mk(inner.bmarkup); elem in iter__next(&iter) {
    		block_markup__check(site, page, elem)
    	}
    } else {
      for iter := iter__mk(inner.imarkup); elem in iter__next(&iter) {
    		inline_markup__check(site, page, elem)
    	}
    }
  case Block_Markup__Aside:
    inline_markup__check(site, page, &inner.title)
    block_markup__check(site, page, &inner.content)
  case Block_Markup__Blockquote:
    block_markup__check(site, page, cast(^Block_Markup)&inner)
  case Table:
    table__check(site, page, &inner)
  case Article_List:
    // NOTE: should we error out if no articles get caught by the filter?
    if inner.heading > MAX_HEADING_LEVEL {
      site__errorf(site, inner.loc, "Invalid heading: %v", inner.heading)
    } else if inner.heading == 0 {
      inner.heading = 2
    }
  }
}
// }}}

// Metadata formatting
// {{{ Posted on...
@(private="file")
fmt__posted_on :: proc(time: ^time.Time) -> Frozen {
  return fmt__freeze1(
    time,
    proc(fi: ^fmt.Info, time: ^time.Time) {
      if mem__is_zero(time^) {
        fmt.wprint(fi.writer, "Being conjured")
      } else {
        fmt.wprintf(fi.writer, "Posted on %v", Datetime__Pretty(time^))
      }
    },
  )
}
// }}}
// {{{ Word count
@(private="file")
fmt__word_count :: proc(word_count: ^uint) -> Frozen {
  return fmt__freeze1(
    word_count,
    proc(fi: ^fmt.Info, wc: ^uint) {
      wc := wc^
      if wc < 400 {
        fmt.wprint(fi.writer, wc)
      } else if wc < 1000 {
        fmt.wprint(fi.writer, wc / 10 * 10)
      } else if wc < 2000 {
        fmt.wprint(fi.writer, wc / 100 * 100)
      } else {
        fmt.wprint(fi.writer, wc / 1000)
      }
    },
  )
}
// }}}
// {{{ Reading duration
@(private="file")
fmt__reading_duration :: proc(word_count: ^uint) -> Frozen {
  return fmt__freeze1(
    word_count,
    proc(fi: ^fmt.Info, wc: ^uint) {
      wc      := wc^
      seconds := wc * 60 / 200
      minutes := wc / 200
      hours   := minutes / 60

      if minutes == 0 {
        fmt.wprintf(fi.writer, "very short %v second", seconds)
      } else if minutes < 10 {
        fmt.wprintf(fi.writer, "short %v minute", minutes)
      } else if wc < 2000 {
        fmt.wprintf(fi.writer, "somewhat short %v minute", minutes)
      } else if wc < 2000 {
        fmt.wprintf(fi.writer, "somewhat long %v minute", minutes)
      } else if wc < 2000 {
        fmt.wprintf(fi.writer, "long %v minute", minutes)
      } else {
        fmt.wprintf(
          fi.writer,
          "very long %v hour and %v minute",
          hours, minutes
        )
      }
    },
  )
}
// }}}
