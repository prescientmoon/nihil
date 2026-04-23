package anima

import "core:fmt"
import "core:log"
import "core:mem"
import "core:mem/virtual"
import "core:os"
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
  title:       IMarkup,
  description: IMarkup,
  content:     BMarkup,

  created_at:   time.Time, // When was the file created?
  published_at: time.Time, // When was the file created?

  // We don't bother defining sitemap-specific types since those options are
  // seldom used in practice.
  changefreq: string, // Override the change frequency for the page
  priority:   string, // Assign a priority to the sitemap entry

  tags:      Exparr(Tag),
  changelog: Exparr(Change),
  headings:  Exparr(^Heading), // The first heading is declared the title
	links:     Exparr(^Def__Link),
	icons:     Exparr(Def__Icon),
  feeds:     Exparr(Def__Feed),
	footnotes: Exparr(^Def__Footnote),
  styles:    Exparr(Def__Stylesheet),
  assets:    Exparr(Def__Asset),
  aliases:   Exparr(Path__Output), // Locations to redirect from
  helmets:   Exparr(Helmet),

  // These attributes are not part of the markup itself
  source_path: Path__Input,
  site_path:   Path__Output,
  url:         URL,
  word_count:  uint, // Merely an estimate!
  in_feeds:    Exparr(^Def__Feed),
  in_styles:   Exparr(^Def__Stylesheet),
  in_helmets:  Exparr(^Helmet),
  uses_LaTeX:  bool,
}

page__make :: proc(allocator: mem.Allocator) -> (page: Page) {
  page.links.allocator      = allocator
  page.icons.allocator      = allocator
  page.headings.allocator   = allocator
  page.footnotes.allocator  = allocator
  page.styles.allocator     = allocator
  page.in_feeds.allocator   = allocator
  page.in_styles.allocator  = allocator
  page.in_helmets.allocator = allocator
  page.assets.allocator     = allocator
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
  imarkup    := codec__imarkup(k)
  bmarkup    := codec__bmarkup(k)
  timestamp  := codec__timestamp(k)
  stylesheet := codec__stylesheet(k)

  return codec__struct(
    k, Page,
    { "feeds",            "feed",             .Exparr, codec__feed(k)      },
    { "tags",             "tag",              .Exparr, codec__tag(k)       },
    { "changelog",        "change",           .Exparr, codec__change(k)    },
    { "aliases",          "alias",            .Exparr, codec__out_path(k)  },
    { "styles",           "stylesheet",       .Exparr, stylesheet         },
    { "assets",           "asset",            .Exparr, codec__asset(k)     },
    { "helmets",          "helmet",           .Exparr, codec__helmet(k)    },
    { "icons",            "deficon",          .Exparr, codec__deficon(k)   },
    { "public",           "public",           .Flag,   true               },
    { "compact",          "compact",          .Flag,   true               },
    { "smaller_headings", "smaller-headings", .Flag,   true               },
    { "content",          {},                 .Some,   bmarkup            },
    { "title",            "title",            .Once,   imarkup            },
    { "description",      "description",      .Maybe,  imarkup            },
    { "filename",         "filename",         .Maybe,  ctext              },
    { "priority",         "priority",         .Maybe,  ctext              },
    { "changefreq",       "changefreq",       .Maybe,  ctext              },
    { "created_at",       "created-at",       .Once,   timestamp          },
    { "published_at",     "published-at",     .Maybe,  timestamp          },
  )
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

    title := imarkup__formatter(g.site, page, &page.title)
    desc  := imarkup__formatter(g.site, page, &page.description)
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

    for iter := iter__mk(page.in_helmets); helmet in iter__next(&iter) {
      if helmet^.format == "html" do xml__raw_string(g, helmet^.content)
    }

    for iter := iter__mk(page.in_styles); style in iter__next(&iter) {
      style := style^
      if style.preload {
        xml__tag(g, "link")
        xml__attr(g, "rel", "preload")
        xml__attr(g, "as", "style")
        xml__attr(g, "href", site__url(g.site, style.site_path, .Stack))
      }

      xml__tag(g, "link")
      xml__attr(g, "rel", "stylesheet")
      xml__attr(g, "href", site__url(g.site, style.site_path, .Stack))
    }

    if xml__tag(g, "title") do imarkup__html(g, page, page.title)

    if mem__non_zero(g.site.favicon) {
      xml__tag(g, "link")
      xml__attr(g, "rel", "icon")
      xml__attr(g, "type", "image/x-icon")
      xml__attr(g, "href", site__url(g.site, g.site.favicon.site_path, .Stack))
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
        bmarkup__anchored_heading(g, page, heading, main = true)
      } else if xml__tag(g, "header") {
        bmarkup__anchored_heading(g, page, heading, main = true)

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
        bmarkup__html(g, page, page.content)
      } else {
        xml__tag(g, "article")
        bmarkup__html(g, page, page.content)
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

  bmarkup__precheck(site, page, &page.content)
  imarkup__check(site, page, &page.description)
  bmarkup__check(site, page, &page.content)

  for iter := iter__mk(page.footnotes); footnote in iter__next(&iter) {
    bmarkup__check(site, page, &footnote^.content)
  }

  for iter := iter__mk(page.links); link in iter__next(&iter) {
    imarkup__check(site, page, &link^.label)
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

    for iter := iter__mk(other_page.helmets); helmet in iter__next(&iter) {
      page_filter__all__eval(other_page^, page^, helmet.scope) or_continue
      push(&page.in_helmets, helmet)
    }
  }

  // Generate feed paths & redirects
  for iter := iter__mk(page.feeds); feed in iter__next(&iter) {
    feed.site_path = site__resolve(site, page.site_path, feed.at)
    imarkup__check(site, page, &feed.description)
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

  level: u8 = 1 // the title is equivalent to a h1
  for iter := iter__mk(page.headings); heading in iter__next(&iter) {
    heading := heading^
    imarkup__check(site, page, &heading.content)

    if heading.level > level + 1 {
      site__errorf(site, heading.loc, "Heading increases level by more than 1")
    }

    level = heading.level

    // Generate ID
    site__frame(site)
    text := fmt.aprint(
      imarkup__formatter(site, page, &heading.content),
      allocator = site__alloc(site, .Stack)
    )

    heading.id = strings.to_delimiter_case(text, '-', false, site__alloc(site))
  }
}

// }}}

// {{{ Changelog entries
Change :: struct {
  at:      time.Time, 
  message: IMarkup,
}

codec__change :: proc(k: ^Codec_Kit) -> ^Codec {
  return codec__struct(
    k, Change,
    { "at",      "at", .Once, codec__timestamp(k) },
    { "message", {},   .Once, codec__imarkup(k)   },
  )
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
Page_Filter__LaTeX  :: distinct Unit // The current page uses LaTeX math

// NOTE: nil is equivalent to Page_Filter__Local
Page_Filter__Atom :: union {
  Page_Filter__All,
  Page_Filter__Any,
  Page_Filter__Not,
  Page_Filter__Tag,
  Page_Filter__Local,
  Page_Filter__Public,
  Page_Filter__LaTeX,
}

// Behaves like \local when empty and like \all otherwise
Page_Filter__Scope :: distinct Page_Filter__All

page_filter__eval :: proc(base, page: Page, filter: Page_Filter__Atom) -> bool {
  switch inner in filter {
  case Page_Filter__Not: 
    if inner == nil do return false
    return !page_filter__eval(base, page, inner^)
  case Page_Filter__Public: return page.public
  case Page_Filter__LaTeX: return page.uses_LaTeX
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
      latex := codec__const(k, "LaTeX", Page_Filter__LaTeX{})
      not := codec__trans_at(k, "not", Page_Filter__Not, codec__ref(k, atom))
      all := codec__trans_at(k, "all", Page_Filter__All, many, {}, {})
      any := codec__trans_at(k, "any", Page_Filter__Any, many, {}, {})
      tag := codec__trans_at(k, "tag", Page_Filter__Tag, codec__tag(k))

      return codec__union(
        k,
        Page_Filter__Atom,
        { Page_Filter__Local,  local  },
        { Page_Filter__Public, public },
        { Page_Filter__LaTeX, latex },
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
  return codec__struct(
    k, Def__Icon,
    { "id",      {},        .Once,  codec__contiguous_text(k)  },
    { "at",      "at",      .Once,  codec__path(k)             },
    { "scope",   "scope",   .Maybe, codec__page_filter__all(k)  },
    { "favicon", "favicon", .Flag,  true                      },
  )
}
// }}}
// {{{ Link definitions
Def__Link :: struct {
	id:     string,
	target: string, // url
	label:  IMarkup,
  scope:  Page_Filter__Scope, // Link definitions can affect other pages
  loc:    Source_Loc,
}

@(private = "file")
codec__deflink :: proc(k: ^Codec_Kit) -> ^Codec {
  return codec__struct(
    k, Def__Link,
    { "target", "target", .Once,  codec__raw(k)              },
    { "label",  "label",  .Maybe, codec__imarkup(k)          },
    { "scope",  "scope",  .Maybe, codec__page_filter__all(k)  },
    { "id",     {},       .Once,  codec__contiguous_text(k)  },
  )
}
// }}}
// {{{ Footnote definitions
Def__Footnote :: struct {
	id:      string,
	content: BMarkup,
  index:   uint, // The page-local number used to display the footnote
  loc:     Source_Loc,
}

@(private = "file")
codec__defnote :: proc(k: ^Codec_Kit) -> ^Codec {
  return codec__struct(
    k, Def__Footnote,
    { "id",      "id", .Once, codec__contiguous_text(k) },
    { "content", {},   .Once, codec__bmarkup(k)         },
  )
}
// }}}
// {{{ Feed definitions
Def__Feed :: struct {
  at:          Path,
  name:        string,
  description: IMarkup,

  members: Page_Filter__All, // What posts should this include?
  under:   Page_Filter__All, // Which pages should this appear on?
  aliases: Exparr(Path__Output), // Locations to redirect from

  // Generated
  site_path: Path__Output,
}

@(private = "file")
codec__feed :: proc(k: ^Codec_Kit) -> ^Codec {
  filter := codec__page_filter__all(k)

  return codec__struct(
    k, Def__Feed,
    { "at",          {},            .Once,   codec__path(k)     },
    { "name",        "name",        .Once,   codec__text(k)     },
    { "description", "description", .Once,   codec__imarkup(k)  },
    { "under",       "under",       .Once,   filter            },
    { "members",     "members",     .Once,   filter            },
    { "aliases",     "alias",       .Exparr, codec__out_path(k) },
  )
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
  return codec__struct(
    k, Def__Stylesheet,
    { "scope",   {},        .Maybe, codec__page_filter__all(k) },
    { "at",      "at",      .Once,  codec__path(k)            },
    { "preload", "preload", .Flag,  true                     },
  )
}
// }}}
// {{{ Assets
Def__Asset :: struct {
  from: Path,
  to:   Path,
}

@(private = "file")
codec__asset :: proc(k: ^Codec_Kit) -> ^Codec {
  return codec__struct(
    k, Def__Asset,
    { "from", {},   .Once,  codec__path(k) },
    { "to",   "to", .Maybe, codec__path(k) },
  )
}
// }}}
// {{{ Helmets
// A helmet is a format-specific metadata blob
Helmet :: struct {
  scope:   Page_Filter__All,
  content: string,
  format:  string, // NOTE: I'm considering making this an enum
}

@(private = "file")
codec__helmet :: proc(k: ^Codec_Kit) -> ^Codec {
  return codec__struct(
    k, Helmet,
    { "content", "content", .Once,  codec__raw(k)             },
    { "format",  "format",  .Once,  codec__contiguous_text(k) },
    { "scope",   {},        .Maybe, codec__page_filter__all(k) },
  )
}
// }}}
// {{{ Headings
MAX_HEADING_LEVEL :: 4
Heading :: struct {
  id:      string,
  content: IMarkup,
  loc:     Source_Loc,
  level:   u8,
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
      outer.level = mem.reinterpret_copy(u8, kit.user_data)
      log.assert(0 < outer.level)
      log.assert(outer.level <= MAX_HEADING_LEVEL)
    }
  }

  looped := codec__struct(
    k, Heading,
    { "content", nil, .Some, codec__imarkup(k)      },
    { "id", "id", .Maybe, codec__contiguous_text(k) },
  )

  with_level := codec__focus(k, Heading, looped, lens, u8(level))
	return codec__loc(k, with_level)
}
// }}}
// {{{ Tables
Table__Cell__Align :: enum u8 { None = 0, Left, Center, Right }
Table__Cell :: struct {
	content: Markup,
  nowrap:  bool,
  align:   Table__Cell__Align,
}

Table__Row :: struct {
	cells: Exparr(Table__Cell),
}

Table__Rowsep :: enum u8 { None = 0, Stripes, Lines }

Table :: struct {
	caption: IMarkup,
	header:  Table__Row,
	rows:    Exparr(Table__Row),
  rowsep:  Table__Rowsep,
}

@(private = "file")
codec__table :: proc(k: ^Codec_Kit) -> ^Codec {
  imarkup := codec__union(k, Markup, { IMarkup, codec__imarkup(k) })
  bmarkup := codec__union(k, Markup, { BMarkup, codec__bmarkup(k) })

  // If used inline, these will become <nil> as any for some reason
  left := Table__Cell__Align.Left
  center := Table__Cell__Align.Center
  right := Table__Cell__Align.Right

	icell := codec__struct(
    k, Table__Cell,
    { "content", nil,      .Maybe, imarkup     },
    { "nowrap",  "nowrap", .Flag,  true        },
    { "align",   "left",   .Flag,  any(left)   },
    { "align",   "center", .Flag,  any(center) },
    { "align",   "right",  .Flag,  any(right)  },
  )

	bcell := codec__struct(
    k, Table__Cell,
    { "content", nil, .Maybe, bmarkup }
  )

  row := codec__struct(
    k, Table__Row,
    { "cells", "bcell",     .Exparr, bcell },
    { "cells", "icell",     .Exparr, icell },
    { "cells", .Double_Bar, .Exparr, bcell },
    { "cells", .Bar,        .Exparr, icell },
  )

  stripes := Table__Rowsep.Stripes
  lines   := Table__Rowsep.Lines
  return codec__struct(
    k, Table,
    { "caption", nil,       .Maybe,  codec__imarkup(k) },
    { "header",  "header",  .Once,   row               },
    { "rows",    "row",     .Exparr, row               },
    { "rowsep",  "stripes", .Flag,   any(stripes)      },
    { "rowsep",  "lines",   .Flag,   any(lines)        },
  )
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
	return codec__loc(k, codec__struct(
    k, Article_List,
    { "filter",  nil,       .Maybe, codec__page_filter__all(k) },
    { "heading", "heading", .Maybe, codec__integer(k, u8)     },
  ))
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
// {{{ Markup
Markup :: union { IMarkup, BMarkup, } // This is currently only used for tables

@(private="file")
markup__html :: proc(g: ^Xml_Gen, page: ^Page, markup: Markup) {
  switch inner in markup {
  case IMarkup: imarkup__html(g, page, inner)
  case BMarkup: bmarkup__html(g, page, inner)
  }
}

@(private="file")
markup__precheck :: proc(site: ^Site, page: ^Page, markup: ^Markup) {
  switch &inner in markup {
  case IMarkup:
  case BMarkup: bmarkup__precheck(site, page, &inner)
  }
}

@(private="file")
markup__check :: proc(site: ^Site, page: ^Page, markup: ^Markup) {
  switch &inner in markup {
  case IMarkup: imarkup__check(site, page, &inner)
  case BMarkup: bmarkup__check(site, page, &inner)
  }
}
// }}}

// {{{ Inline markup
IMarkup__Space :: distinct Unit
IMarkup__Ellipsis :: distinct Unit
IMarkup__Text :: distinct string
IMarkup__Emph :: distinct IMarkup
IMarkup__Strong :: distinct IMarkup
IMarkup__Strikethrough :: distinct IMarkup
IMarkup__Mono :: distinct string
IMarkup__Quote :: distinct IMarkup

IMarkup__Icon :: struct {
  id:  string,
  loc: Source_Loc,
  def: ^Def__Icon, // Inserted after the fact
}

IMarkup__Fn :: struct {
  id:  string,
  loc: Source_Loc,
  def: ^Def__Footnote, // Inserted after the fact
}

IMarkup__Link :: struct {
	id:    string,
	label: IMarkup,
  loc:   Source_Loc,
  def:   ^Def__Link, // Inserted after the fact
}

IMarkup__Timestamp :: struct {
  time: time.Time,
  compact: bool, // Shortens the output
}

IMarkup__Date :: distinct IMarkup__Timestamp
IMarkup__Datetime :: distinct IMarkup__Timestamp
IMarkup__LaTeX :: distinct string

// Using distinct runs into circular types issue (for no reason)
IMarkup :: struct {
  // We store a pointer here to drastically reduce the size of the struct. We
  // *could* change every usage site to ^IMarkup, but this is simpler for now
	elements: ^Exparr(IMarkup__Atom),
}

// This currently takes up a whole 32B, which is a bit annoying. We could get
// this as low as 16B by simply using pointers for a bunch of the branches. I
// will bother doing so once the memory usage goes past 1MiB.
IMarkup__Atom :: union {
	IMarkup__Space,
	IMarkup__Ellipsis,
	IMarkup__Text,
	IMarkup__Emph,
	IMarkup__Strong,
	IMarkup__Strikethrough,
	IMarkup__Mono,
	IMarkup__Quote,
	IMarkup__Date,
	IMarkup__Datetime,
	IMarkup__LaTeX,
	^IMarkup__Icon,
	^IMarkup__Fn,
	^IMarkup__Link,
}
// }}}
// {{{ Codecs
@(private = "file")
codec__imarkup__atom :: proc(
  k: ^Codec_Kit
) -> ^Codec {
	imarkup := codec__imarkup(k)
	ctext := codec__contiguous_text(k)

	space := codec__space(k, IMarkup__Space{})
	text := codec__transmute(k, IMarkup__Text, codec__string(k))
	ellipsis__sugar := codec__token(k, .Ellipsis, IMarkup__Ellipsis{})
	ellipsis__basic := codec__const(k, "...", IMarkup__Ellipsis{})

  emph := codec__transmute(k, IMarkup__Emph, imarkup)
	emph__sugar := codec__delim(k, .Underscore, .Underscore, emph)
	emph__basic := codec__at(k, "emph", emph)

  strong := codec__transmute(k, IMarkup__Strong, imarkup)
	strong__sugar := codec__delim(k, .Asterisk, .Asterisk, strong)
	strong__basic := codec__at(k, "strong", strong)

  strike := codec__transmute(k, IMarkup__Strikethrough, imarkup)
	strike__sugar := codec__delim(k, .Tilde, .Tilde, strike)
	strike__basic := codec__at(k, "strike", strike)

  mono := codec__transmute(k, IMarkup__Mono, codec__raw(k))
	mono__sugar := codec__delim(k, .Backtick, .Backtick, mono)
	mono__basic := codec__at(k, "mono", mono)

  math := codec__transmute(k, IMarkup__LaTeX, codec__raw(k))
	math__sugar := codec__delim(k, .Dollar, .Dollar, math)
	math__basic := codec__at(k, "imath", math)

  quote := codec__transmute(k, IMarkup__Quote, imarkup)
	quote__sugar := codec__delim(k, .Quote, .Quote, quote)
	quote__basic := codec__at(k, "quote", quote)

  icon_id := codec__field(k, "id", IMarkup__Icon, ctext, ONCE)
  icon := codec__ref(k, codec__at(k, "icon", codec__loc(k, icon_id)))

  fn_id := codec__field(k, "id", IMarkup__Fn, ctext, ONCE)
  fn := codec__ref(k, codec__at(k, "fn", codec__loc(k, fn_id)))

  Link :: IMarkup__Link
  link_id := codec__field(k, "id", Link, ctext, ONCE)
  link_label__basic := codec__at(k, "label", imarkup)
  link_label__sugar := codec__leaded(k, .Bar, imarkup)
  link_label__payload := codec__sum(k, link_label__basic, link_label__sugar)
  link_label := codec__field(k, "label", Link, link_label__payload, UNIQUE)
  link_sum := codec__sum(k, link_label, link_id)
  link_payload := codec__loc(k, codec__loop(k, link_sum))
  link__sugar := codec__ref(k, codec__at(k, "link", link_payload))
  link__basic := codec__ref(k, codec__delim(k, .LSquare, .RSquare, link_payload))

  timestamp :=  codec__struct(
    k, IMarkup__Timestamp,
    { "time",    nil,       .Once, codec__timestamp(k) },
    { "compact", "compact", .Flag, true               },
  )

  date := codec__trans_at(k, "date", IMarkup__Date, timestamp)
  datetime := codec__trans_at(k, "datetime", IMarkup__Datetime, timestamp)

	return codec__union(
		k,
    IMarkup__Atom,
    { IMarkup__Space,         space          },
    { IMarkup__Text,          text           },
    { IMarkup__Ellipsis,      ellipsis__sugar },
    { IMarkup__Ellipsis,      ellipsis__basic },
    { IMarkup__Emph,          emph__sugar     },
    { IMarkup__Emph,          emph__basic     },
    { IMarkup__Strong,        strong__sugar   },
    { IMarkup__Strong,        strong__basic   },
    { IMarkup__Strikethrough, strike__sugar   },
    { IMarkup__Strikethrough, strike__basic   },
    { IMarkup__Mono,          mono__sugar     },
    { IMarkup__Mono,          mono__basic     },
    { IMarkup__LaTeX,         math__sugar     },
    { IMarkup__LaTeX,         math__basic     },
    { IMarkup__Quote,         quote__sugar    },
    { IMarkup__Quote,         quote__basic    },
    { IMarkup__Date,          date           },
    { IMarkup__Datetime,      datetime       },
    { ^IMarkup__Icon,         icon           },
    { ^IMarkup__Fn,           fn             },
    { ^IMarkup__Link,         link__sugar     },
    { ^IMarkup__Link,         link__basic     },
	)
}

@(private="file")
codec__imarkup :: proc(kit: ^Codec_Kit) -> ^Codec {
	return codec__memo(
		kit,
		"imarkup",
    IMarkup,
		proc(k: ^Codec_Kit) -> ^Codec {
      // This lens marks runs only consisting of spaces as ignored.
      lens :: proc(kit: ^Lens_Kit) {
        switch kit.mode {
        case .Project:
          mem.copy(kit.inner, kit.outer, size_of(IMarkup))
        case .Inject:
          inner := cast(^^Exparr(IMarkup__Atom))kit.inner

          found_substantial := false
          for iter := iter__mk(inner^^); elem in iter__next(&iter) {
            if _, ok := elem.(IMarkup__Space); !ok {
              found_substantial = true
              break
            }
          }

          if found_substantial {
            mem.copy(kit.outer, kit.inner, size_of(IMarkup))
          } else {
            kit.ignored = true
          }
        }
      }

			return codec__focus(
				k,
				IMarkup,
        codec__ref(k, codec__exparr(k, codec__imarkup__atom(k))),
        lens
			)
		},
	)
}
// }}}
// {{{ Formatting as text
@(private="file")
imarkup__atom__fmt :: proc(
  fi: ^fmt.Info, site: Site, page: Page, atom: IMarkup__Atom
) {
  switch inner in atom {
  case ^IMarkup__Icon, nil:
  case IMarkup__Space:
    fmt.wprint(fi.writer, " ")
  case IMarkup__Ellipsis:
    fmt.wprint(fi.writer, ELLIPSIS_SYMBOL)
  case IMarkup__Text:
    fmt.wprint(fi.writer, string(inner))
  case IMarkup__Emph:
    fmt.wprint(fi.writer, "_")
    imarkup__fmt(fi, site, page, IMarkup(inner))
    fmt.wprint(fi.writer, "_")
  case IMarkup__Strong:
    fmt.wprint(fi.writer, "*")
    imarkup__fmt(fi, site, page, IMarkup(inner))
    fmt.wprint(fi.writer, "*")
  case IMarkup__Strikethrough:
    fmt.wprint(fi.writer, "~")
    imarkup__fmt(fi, site, page, IMarkup(inner))
    fmt.wprint(fi.writer, "~")
  case IMarkup__Mono:
    fmt.wprintf(fi.writer, "`%v`", string(inner))
  case IMarkup__LaTeX:
    fmt.wprintf(fi.writer, "`$%v$`", string(inner))
  case IMarkup__Quote:
    fmt.wprint(fi.writer, QUOTE_EN_LEFT)
    imarkup__fmt(fi, site, page, IMarkup(inner))
    fmt.wprint(fi.writer, QUOTE_EN_RIGHT)
  case ^IMarkup__Link:
    if mem__non_zero(inner.label) {
      imarkup__fmt(fi, site, page, inner.label)
    } else if inner.def != nil && mem__non_zero(inner.def.label) {
      imarkup__fmt(fi, site, page, inner.def.label)
    } else if inner.def != nil {
      fmt.wprintf(fi.writer, inner.def.id)
    } else {
      fmt.wprint(fi.writer, inner.id)
    }
  case ^IMarkup__Fn:
    if inner.def != nil {
      fmt.wprintf(fi.writer, "[^%v]", inner.def.index)
    } else {
      fmt.wprint(fi.writer, ERROR_TEXT)
    }
  case IMarkup__Date:
    if inner.compact {
      fmt.wprintf(fi.writer, "%v", Date__Compact(inner.time))
    } else {
      fmt.wprintf(fi.writer, "%v", Date__Pretty(inner.time))
    }
  case IMarkup__Datetime:
    if inner.compact {
      fmt.wprintf(fi.writer, "%v", Datetime__Compact(inner.time))
    } else {
      fmt.wprintf(fi.writer, "%v", Datetime__Pretty(inner.time))
    }
  }
}

@(private="file")
imarkup__fmt :: proc(
  fi: ^fmt.Info, site: Site, page: Page, im: IMarkup
) {
  if im.elements == nil do return
  for iter := iter__mk(im.elements^); chunk in iter__next(&iter) {
    imarkup__atom__fmt(fi, site, page, chunk^)
  }
}

imarkup__formatter :: proc(
  site: ^Site, page: ^Page, im: ^IMarkup
) -> Frozen {
  return fmt__freeze3(
    site, 
    page, 
    im,
    proc(fi: ^fmt.Info, site: ^Site, page: ^Page, im: ^IMarkup) {
      imarkup__fmt(fi, site^, page^, im^)
    },
  )
}
// }}}
// {{{ Formatting as html
@(private="file")
imarkup__atom__html :: proc(
  g: ^Xml_Gen, page: ^Page, atom: IMarkup__Atom
) {
  switch inner in atom {
  case nil:
  case ^IMarkup__Icon:
    xml__attr(g, "class", "icon")
    // Decorative image.
    // See: https://www.w3.org/WAI/tutorials/images/decorative/
    xml__attr(g, "alt", "")
    xml__attr(g, "src", site__url(g.site, inner.def.site_path, .Stack))
  case IMarkup__Space:
    xml__string(g, " ")
  case IMarkup__Ellipsis:
    xml__stringf(g, "%v", ELLIPSIS_SYMBOL)
  case IMarkup__Text:
    xml__string(g, string(inner))
  case IMarkup__Emph:
    xml__tag(g, "em")
    imarkup__html(g, page, IMarkup(inner))
  case IMarkup__Strong:
    xml__tag(g, "strong")
    imarkup__html(g, page, IMarkup(inner))
  case IMarkup__Strikethrough:
    xml__tag(g, "s")
    imarkup__html(g, page, IMarkup(inner))
  case IMarkup__Mono:
    xml__tag(g, "code")
    xml__string(g, string(inner))
  case IMarkup__LaTeX:
    site__frame(g.site)
    xml__raw_string(g, render_math(g.site, .LaTeX_Inline, string(inner)))
  case IMarkup__Quote:
    xml__stringf(g, "%v", QUOTE_EN_LEFT)
    imarkup__html(g, page, IMarkup(inner))
    xml__stringf(g, "%v", QUOTE_EN_RIGHT)
  case ^IMarkup__Link:
    xml__tag(g, "a")
    xml__attr(g, "href", inner.def.target)
    if mem__non_zero(inner.label) {
      imarkup__html(g, page, inner.label)
    } else if inner.def != nil && mem__non_zero(inner.def.label) {
      imarkup__html(g, page, inner.def.label)
    } else if inner.def != nil {
      xml__string(g, inner.def.id)
    } else {
      xml__string(g, inner.id)
    }
  case ^IMarkup__Fn:
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
  case IMarkup__Date:
    xml__tag(g, "time")
    xml__attrf(g, "datetime", "%v", Rfc3339(inner.time))
    if inner.compact {
      xml__stringf(g, "%v", Date__Compact(inner.time))
    } else {
      xml__stringf(g, "%v", Date__Pretty(inner.time))
    }
  case IMarkup__Datetime:
    xml__tag(g, "time")
    xml__attrf(g, "datetime", "%v", Rfc3339(inner.time))
    if inner.compact {
      xml__stringf(g, "%v", Datetime__Compact(inner.time))
    } else {
      xml__stringf(g, "%v", Datetime__Pretty(inner.time))
    }
  }
}

imarkup__html :: proc(
  g: ^Xml_Gen, page: ^Page, im: IMarkup
) {
  if im.elements == nil do return
  for iter := iter__mk(im.elements^); chunk in iter__next(&iter) {
    imarkup__atom__html(g, page, chunk^)
  }
}
// }}}
// {{{ Checking
@(private="file")
imarkup__check :: proc(site: ^Site, page: ^Page, im: ^IMarkup) {
  if im.elements == nil do return

  // Remove spurious leading/trailing spaces.
  for _ in 0..<2 {
    exparr__reverse(im.elements^)
    for chunk in exparr__try_last(im.elements^) {
      _ = chunk.(IMarkup__Space) or_break
      exparr__pop(im.elements)
    }
  }

  for iter := iter__mk(im.elements^); chunk in iter__next(&iter) {
    imarkup__atom__check(site, page, chunk)
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
imarkup__atom__check :: proc(
  site: ^Site, page: ^Page, atom: ^IMarkup__Atom
) {
  switch &inner in atom {
  case nil:
  case IMarkup__Space:
  case IMarkup__Ellipsis:
  case IMarkup__Date: 
  case IMarkup__Datetime: 
  case IMarkup__Mono:
  case IMarkup__LaTeX:
    page.uses_LaTeX = true
  case IMarkup__Text:
    for char in string(inner) {
      unicode.is_alpha(char) or_continue
      page.word_count += 1
      break
    }
  case IMarkup__Emph:
    imarkup__check(site, page, cast(^IMarkup)&inner)
  case IMarkup__Strong:
    imarkup__check(site, page, cast(^IMarkup)&inner)
  case IMarkup__Strikethrough:
    imarkup__check(site, page, cast(^IMarkup)&inner)
  case IMarkup__Quote:
    imarkup__check(site, page, cast(^IMarkup)&inner)
  case ^IMarkup__Link:
    log.assert(inner.def == nil)

    if mem__non_zero(inner.label) {
      imarkup__check(site, page, &inner.label)
    }

    site__frame(site)
    options := Exparr(Source_Loc) { allocator = site__alloc(site, .Stack) }

    for iter := iter__mk(site.pages); defsite in iter__next(&iter) {
      for iter := iter__mk(defsite.links); link in iter__next(&iter) {
        link := link^
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
  case ^IMarkup__Fn:
    log.assert(inner.def == nil)

    site__frame(site)
    options := Exparr(Source_Loc) { allocator = site__alloc(site, .Stack) }

    for iter := iter__mk(page.footnotes); footnote in iter__next(&iter) {
      (footnote^.id == inner.id) or_continue
      push(&options, footnote^.loc)
      inner.def = footnote^
    }

    if inner.def == nil {
      site__errorf(site, inner.loc, "Footnote '%v' is not in scope.", inner.id)
    } else if options.len > 1 {
      ambiguous_reference_error(site, inner.id, inner.loc, options)
    }

  case ^IMarkup__Icon:
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
BMarkup__Paragraph :: distinct IMarkup

BMarkup__Image :: struct {
	alt:          IMarkup,
	source:       Path,
  out_path:     Path__Output,
  loc:          Source_Loc,
  width:        uint,
  height:       uint,
  visual_width: string,
  pixelated:    bool,
}

BMarkup__Figure :: struct {
	caption: IMarkup,
	content: BMarkup,
}

BMarkup__BList :: struct {
	ordered:  bool,
  elements: Exparr(BMarkup),
}

BMarkup__IList :: struct {
	ordered:  bool,
  elements: Exparr(IMarkup),
}

BMarkup__Aside :: struct {
  id:       string,
  char:     string, // Icon name
  content:  BMarkup,
  title:    IMarkup,

  // Whether to hide the content by default
  collapse: bool,
}

BMarkup__Code :: struct {
  language: string,
  content:  string,
}

BMarkup__Blockquote :: distinct BMarkup
BMarkup__Description :: distinct Unit
BMarkup__Table_Of_Contents :: distinct Unit
BMarkup__Thematic_Break :: distinct Unit

// These are inserted after the fact, during the checking phase
BMarkup__Section :: struct {
  heading: ^Heading,
  content: BMarkup,
}

// This currently takes up a fat 144B. If memory usage ever goes past 1MiB, I
// will bother using pointers for the various branches, thus not wasting so much
// space on padding.
BMarkup__Atom :: union {
	BMarkup__Paragraph,
	BMarkup__Image,
	BMarkup__Figure,
	BMarkup__IList,
	BMarkup__BList,
	BMarkup__Blockquote,
	BMarkup__Description,
	BMarkup__Table_Of_Contents,
	BMarkup__Thematic_Break,
  BMarkup__Aside,
  BMarkup__Code,
  BMarkup__Section,
  Article_List,
	Table,
  Def__Link,
  Def__Footnote,
  Heading,
}

BMarkup :: struct {
	elements: Exparr(BMarkup__Atom),
}
// }}}
// {{{ Codecs
@(private = "file")
codec__bmarkup__image :: proc(
  k: ^Codec_Kit
) -> ^Codec {
  return codec__loc(k, codec__struct(
    k, BMarkup__Image,
    { "alt",          .Bar,        .Maybe, codec__imarkup(k)         },
    { "alt",          "alt",       .Maybe, codec__imarkup(k)         },
    { "source",       {},          .Once,  codec__path(k)            },
    { "visual_width", "width",     .Maybe, codec__contiguous_text(k) },
    { "pixelated",    "pixelated", .Flag,  true                     },
  ))
}

@(private = "file")
codec__bmarkup__figure :: proc(
  k: ^Codec_Kit
) -> ^Codec {
  imarkup := codec__imarkup(k)
  bmarkup := codec__bmarkup(k)
	caption := codec__field_at(k, "caption", BMarkup__Figure, imarkup, UNIQUE)
	content := codec__field(k, "content", BMarkup__Figure, bmarkup, ONCE)
	return codec__loop(k, codec__sum(k, caption, content))
}

@(private = "file")
codec__bmarkup__aside :: proc(
  k: ^Codec_Kit
) -> ^Codec {
  Self :: BMarkup__Aside

  ctext   := codec__contiguous_text(k)
  imarkup := codec__imarkup(k)
  bmarkup := codec__bmarkup(k)

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

codec__bmarkup__blist :: proc(k: ^Codec_Kit) -> ^Codec {
  return codec__struct(
    k, BMarkup__BList,
    { "ordered",  "ordered", .Flag,   true             },
    { "elements", "item",    .Exparr, codec__bmarkup(k) },
    { "elements", .Asterisk, .Exparr, codec__bmarkup(k) },
  )
}

codec__bmarkup__ilist :: proc(k: ^Codec_Kit) -> ^Codec {
  return codec__struct(
    k, BMarkup__BList,
    { "ordered",  "ordered", .Flag,   true             },
    { "elements", "item",    .Exparr, codec__imarkup(k) },
    { "elements", .Asterisk, .Exparr, codec__imarkup(k) },
  )
}

codec__bmarkup__code :: proc(k: ^Codec_Kit) -> ^Codec {
  return codec__struct(
    k, BMarkup__Code,
    { "language", nil,       .Once, codec__contiguous_text(k) },
    { "content",  "content", .Once, codec__raw(k)             },
  )
}

@(private = "file")
codec__bmarkup__atom :: proc(
  k: ^Codec_Kit
) -> ^Codec {
	imarkup := codec__imarkup(k)
	bmarkup := codec__bmarkup(k)

	description := codec__const(k, "embed-description", BMarkup__Description{})
	thematic_break := codec__const(k, "---", BMarkup__Thematic_Break{})
	table_of_contents := codec__const(k, "toc", BMarkup__Table_Of_Contents{})

	blockquote := codec__transmute(k, BMarkup__Blockquote, bmarkup)
  blockquote__sugar := codec__leaded(k, .GT, blockquote)
  blockquote__basic := codec__at(k, ">", blockquote)

	image := codec__at(k, "image", codec__bmarkup__image(k))
	figure := codec__at(k, "figure", codec__bmarkup__figure(k))
	para := codec__transmute(k, BMarkup__Paragraph, codec__para(k, imarkup))
	table := codec__at(k, "table", codec__table(k))
	deflink := codec__at(k, "deflink", codec__deflink(k))
	defnote := codec__at(k, "defnote", codec__defnote(k))
	aside := codec__at(k, "aside", codec__bmarkup__aside(k))
  article_list := codec__at(k, "index", codec__article_list(k))
  ilist := codec__at(k, "ilist", codec__bmarkup__ilist(k))
  blist := codec__at(k, "blist", codec__bmarkup__blist(k))
  code := codec__at(k, "code", codec__bmarkup__code(k))

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
    BMarkup__Atom,
    { BMarkup__Blockquote,        blockquote__sugar  },
    { BMarkup__Blockquote,        blockquote__basic  },
    { BMarkup__Description,       description       },
    { BMarkup__Table_Of_Contents, table_of_contents },
    { BMarkup__Thematic_Break,    thematic_break    },
    { BMarkup__Image,             image             },
    { BMarkup__Figure,            figure            },
    { Table,                     table             },
    { Heading,                   h2__sugar          },
    { Heading,                   h3__sugar          },
    { Heading,                   h4__sugar          },
    { Heading,                   h2__basic          },
    { Heading,                   h3__basic          },
    { Heading,                   h4__basic          },
    { Article_List,              article_list      },
    { BMarkup__IList,             ilist             },
    { BMarkup__BList,             blist             },
    { BMarkup__Code,              code              },
    { BMarkup__Aside,             aside             },
    { Def__Link,                  deflink           },
    { Def__Footnote,              defnote           },
    { BMarkup__Paragraph,         para              },
	)
}

@(private="file")
codec__bmarkup :: proc(kit: ^Codec_Kit) -> ^Codec {
	return codec__memo(
		kit,
		"bmarkup",
    BMarkup,
		proc(kit: ^Codec_Kit) -> ^Codec {
			return codec__transmute(
				kit,
				BMarkup,
				codec__spaced_exparr(kit, codec__bmarkup__atom(kit)),
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
bmarkup__anchored_heading :: proc(
  g: ^Xml_Gen, page: ^Page, heading: Heading, main := false
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
  imarkup__html(g, page, heading.content)
}

@(private="file")
bmarkup__atom__html :: proc(
  g: ^Xml_Gen, page: ^Page, atom: BMarkup__Atom
) {
  switch &inner in atom {
  case nil, Def__Link, Def__Footnote:
  case BMarkup__Thematic_Break:
    xml__tag(g, "hr", single = true)
  case Heading:
    log.panic("Cannot render section-less heading as HTML")
  case BMarkup__Description:
    xml__tag(g, "p")
    imarkup__html(g, page, page.description)
  case BMarkup__Table_Of_Contents:
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
    stack: [dynamic; MAX_HEADING_LEVEL]u8
    // Whether we've created an <ol> element for the top of the stack. Since an
    // empty stack is contained in the <ol> we've just created above, this
    // starts out as being true.
    last_has_children := true
    for iter := iter__mk(page.headings); heading in iter__next(&iter) {
      heading := heading^
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
      imarkup__html(g, page, heading.content)
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
      page_filter__eval(page^, article^, inner.filter) or_continue

      xml__tag(g, "li")
      xml__tag(g, "article")

      if xml__tag(g, HEADING_TAG_NAMES[inner.heading - 1]) {
        xml__tag(g, "a")
        xml__attrf(g, "href", "%v", article.url)
        xml__attr(g, "rel", "bookmark")
        imarkup__html(g, article, article.title)
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
      imarkup__html(g, article, article.description)
    }
  case BMarkup__Section:
    xml__tag(g, "section")
    xml__attr(g, "aria-labelledby", inner.heading.id)
    bmarkup__anchored_heading(g, page, inner.heading^)
    bmarkup__html(g, page, inner.content)
  case BMarkup__Paragraph:
    xml__tag(g, "p")
    imarkup__html(g, page, IMarkup(inner))
  case BMarkup__Blockquote:
    xml__tag(g, "blockquote")
    bmarkup__html(g, page, BMarkup(inner))
  case BMarkup__IList:
    xml__tag(g, inner.ordered ? "ol" : "ul")
    for iter := iter__mk(inner.elements); elem in iter__next(&iter) {
      xml__tag(g, "li")
      imarkup__html(g, page, elem^)
    }
  case BMarkup__BList:
    xml__tag(g, inner.ordered ? "ol" : "ul")
    for iter := iter__mk(inner.elements); elem in iter__next(&iter) {
      xml__tag(g, "li")
      bmarkup__html(g, page, elem^)
    }
  case BMarkup__Code:
    xml__tag(g, "pre")
    xml__tag(g, "code")
    xml__attr(g, "data-language", inner.language)
    xml__string(g, inner.content)
  case BMarkup__Aside:  // TODO
  case BMarkup__Image:
    xml__tag(g, "img", single = true)
    xml__attrf(g, "src", "/%v", inner.out_path)
    xml__attr(g, "width", inner.width)
    xml__attr(g, "height", inner.height)

    if mem__non_zero(inner.alt) {
      xml__attr(g, "alt", imarkup__formatter(g.site, page, &inner.alt))
    }

    if mem__non_zero(inner.visual_width) {
      xml__attrf(g, "style", "width: %v", inner.visual_width)
    }

    if inner.pixelated do xml__attr(g, "class", "pixelated")
  case BMarkup__Figure:
    xml__tag(g, "figure")
    if mem__non_zero(inner.caption) {
      xml__tag(g, "figcaption")
      imarkup__html(g, page, inner.caption)
    }

    bmarkup__html(g, page, inner.content)
  case Table:
    xml__tag(g, "table")

    switch inner.rowsep {
    case .None:
    case .Stripes: xml__attr(g, "class", "rowsep-stripes")
    case .Lines:   xml__attr(g, "class", "rowsep-lines")
    }

    if mem__non_zero(inner.caption) {
      xml__tag(g, "caption")
      imarkup__html(g, page, inner.caption)
    }

    row__html :: proc(
      g: ^Xml_Gen, page: ^Page, header, row: Table__Row, kind: string
    ) {
      xml__tag(g, "tr")
      for iter := iter__mk(row.cells); cell, i in iter__next(&iter) {
        header_cell := exparr__get(header.cells, i)
        xml__tag(g, kind)

        classes: [dynamic; 2]string

        nowrap := cell.nowrap || header_cell.nowrap
        if nowrap do push(&classes, "nowrap")

        align := cell.align
        if align == .None do align = header_cell.align
        switch align {
        case .None:
        case .Left:   push(&classes, "align-left")
        case .Center: push(&classes, "align-center")
        case .Right:  push(&classes, "align-right")
        }

        if len(classes) != 0 {
          site__frame(g.site)
          classname := strings.join(classes[:], " ", site__alloc(g.site, .Stack))
          xml__attr(g, "class", classname)
        }

        markup__html(g, page, cell.content)
      }
    }

    if xml__tag(g, "thead") {
      row__html(g, page, inner.header, inner.header, "th")
    }

    xml__tag(g, "tbody")
    for iter := iter__mk(inner.rows); row in iter__next(&iter) {
      row__html(g, page, inner.header, row^, "td")
    }
  }
}

bmarkup__html :: proc(
  g: ^Xml_Gen, page: ^Page, bm: BMarkup
) {
  for iter := iter__mk(bm.elements); chunk in iter__next(&iter) {
    bmarkup__atom__html(g, page, chunk^)
  }
}
// }}}
// {{{ Pre-checking
@(private="file")
bmarkup__precheck :: proc(site: ^Site, page: ^Page, bm: ^BMarkup) {
  if bm == nil do return
  for iter := iter__mk(bm.elements); atom in iter__next(&iter) {
    bmarkup__atom__precheck(site, page, atom)
  }
}

@(private="file")
bmarkup__atom__precheck :: proc(
  site: ^Site, page: ^Page, atom: ^BMarkup__Atom
) {
  switch &inner in atom {
  case Def__Footnote: push(&page.footnotes, &inner)
  case Def__Link:     push(&page.links,     &inner)
  case Heading:      push(&page.headings,  &inner)
  case nil, BMarkup__Code, BMarkup__Description, BMarkup__Table_Of_Contents,
       BMarkup__Thematic_Break, Article_List, BMarkup__Paragraph,
       BMarkup__Image, BMarkup__IList:
  case BMarkup__Section:
    bmarkup__precheck(site, page, &inner.content)
  case BMarkup__Figure:
    bmarkup__precheck(site, page, &inner.content)
  case BMarkup__Aside:
    bmarkup__precheck(site, page, &inner.content)
  case BMarkup__Blockquote:
    bmarkup__precheck(site, page, cast(^BMarkup)&inner)
  case BMarkup__BList:
    for iter := iter__mk(inner.elements); elem in iter__next(&iter) {
      bmarkup__precheck(site, page, elem)
    }
  case Table:
    table__row__precheck :: proc(site: ^Site, page: ^Page, row: ^Table__Row) {
      for iter := iter__mk(row.cells); cell in iter__next(&iter) {
        markup__precheck(site, page, &cell.content)
      }
    }

    table__row__precheck(site, page, &inner.header)
    for iter := iter__mk(inner.rows); row in iter__next(&iter) {
      table__row__precheck(site, page, row)
    }
  }
}
// }}}
// {{{ Checking
@(private="file")
bmarkup__check :: proc(site: ^Site, page: ^Page, bm: ^BMarkup) {
  if bm == nil do return
  for iter := iter__mk(bm.elements); atom in iter__next(&iter) {
    bmarkup__atom__check(site, page, atom)
  }

  // Split block into sections We don't attempt to reuse any of the existing
  // structure... As such, we try to avoind neednessly creating new copies when
  // the body contains no headings.
  has_headings: bool
  for iter := iter__mk(bm.elements); atom in iter__next(&iter) {
    heading := atom.(Heading) or_continue
    has_headings = true
    break
  }

  if !has_headings do return

  sectioned: BMarkup // The root section we write to
  sectioned.elements.allocator = site__alloc(site, .Forever)

  stack: [dynamic; MAX_HEADING_LEVEL]^BMarkup__Section
  for iter := iter__mk(bm.elements); atom in iter__next(&iter) {
    if heading, ok := &atom.(Heading); ok {
      #reverse for last in stack {
        (last.heading.level >= heading.level) or_break
        pop(&stack)
      }

      inner_content: BMarkup
      inner_content.elements.allocator = site__alloc(site, .Forever)
      atom := BMarkup__Section {
        heading = heading,
        content = inner_content,
      }

      ref: ^BMarkup__Atom
      if l := len(stack); l > 0 {
        last := stack[l - 1]
        ref = push(&last.content.elements, atom)
      } else {
        ref = push(&sectioned.elements, atom)
      }

      section := &ref.(BMarkup__Section)
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
bmarkup__atom__check :: proc(
  site: ^Site, page: ^Page, atom: ^BMarkup__Atom
) {
  switch &inner in atom {
  case nil, BMarkup__Code, BMarkup__Description,
       BMarkup__Table_Of_Contents, BMarkup__Thematic_Break,
       Def__Link, Def__Footnote, Heading:
  case BMarkup__Section:
    bmarkup__check(site, page, &inner.content)
  case BMarkup__Paragraph:
    imarkup__check(site, page, cast(^IMarkup)&inner)
  case BMarkup__Image:
    imarkup__check(site, page, &inner.alt)

    found := false
    for extension in ([]string { "", ".webp", ".jpg", ".png" }) {
      site__frame(site)
      source := cast(Path)fmt.aprintf(
        "%v%v",
        inner.source,
        extension,
        allocator = site__alloc(site, .Stack),
      )

      candidate := site__resolve(site, page.source_path, source, .Stack)
      absolute := site__absolute(site, site.content_root, candidate, .Stack)
      os.exists(string(absolute)) or_continue

      // We only store this off the stack when it's actually needed
      clone := strings__clone(candidate, site__alloc(site))
      inner.out_path = Path__Output(clone)

      dimensions := image_dimensions(string(absolute))
      inner.width  = dimensions.width
      inner.height = dimensions.height

      source_clone := strings__clone(source, site__alloc(site))
      push(&page.assets, Def__Asset { source_clone, source_clone })

      found = true
      break
    }

    if !found {
      site__errorf(site, inner.loc, "Cannot find image %v", inner.source)
    }
  case BMarkup__Figure:
    imarkup__check(site, page, &inner.caption)
    bmarkup__check(site, page, &inner.content)
  case BMarkup__IList:
    for iter := iter__mk(inner.elements); elem in iter__next(&iter) {
      imarkup__check(site, page, elem)
    }
  case BMarkup__BList:
    for iter := iter__mk(inner.elements); elem in iter__next(&iter) {
      bmarkup__check(site, page, elem)
    }
  case BMarkup__Aside:
    imarkup__check(site, page, &inner.title)
    bmarkup__check(site, page, &inner.content)
  case BMarkup__Blockquote:
    bmarkup__check(site, page, cast(^BMarkup)&inner)
  case Table:
    table__row__check :: proc(site: ^Site, page: ^Page, row: ^Table__Row) {
      for iter := iter__mk(row.cells); cell in iter__next(&iter) {
        markup__check(site, page, &cell.content)
      }
    }

    imarkup__check(site, page, &inner.caption)
    table__row__check(site, page, &inner.header)
    for iter := iter__mk(inner.rows); row in iter__next(&iter) {
      table__row__check(site, page, row)
    }
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

// Rust utils
// {{{ Math rendering
@(private="file")
Math_Mode :: enum u8 {
	LaTeX_Inline = 1,
	LaTeX_Block = 2,
}

@(private="file")
Render_Math_Input :: struct {
	input: string,
	output: ^string,
	mode: Math_Mode,
}

@(private="file")
Render_Math_Output :: struct {
	required_size: uint, // When 0 => we have enough memory
}

@(private="file")
render_math :: proc(site: ^Site, mode: Math_Mode, input: string) -> string {
  size := uint(1 * mem.Kilobyte)
  allocator := site__alloc(site, .Stack)
  output: mem.Raw_String
  byte := mem__layout(byte)
  for {
    output.data = cast([^]u8)mem__resize(
      output.data,
      layout__array(byte, output.len),
      layout__array(byte, size),
      allocator,
    )

    output.len = int(size)

    result := rust__render_math({ 
      mode   = mode,
      input  = input,
      output = cast(^string)&output,
    })

    if result.required_size == 0 {
      return transmute(string)output
    } else {
      size = uint(result.required_size)
      log.infof("The math renderer requested a larger buffer: %v", Bytes(size))
    }
  }
}
// }}}
// {{{ Image dimensions
@(private="file")
Image_Dimensions :: struct {
	width, height: uint,
}
// }}}
// {{{ Foregin imports
foreign import rust_utils "system:libanima_rust_utils.a"

@(default_calling_convention="c")
foreign rust_utils {
  @(private="file")
  @(link_name="render_math")
	rust__render_math :: proc(args: Render_Math_Input) -> Render_Math_Output ---

  @(private="file")
  @(link_name="image_dimensions")
	image_dimensions :: proc(path: string) -> Image_Dimensions ---
}

// }}}
