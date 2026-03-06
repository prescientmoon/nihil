package anima

import "core:os"
import "core:fmt"
import "core:mem"
import "core:log"
import "core:time"
import "core:strings"
import "core:mem/virtual"

// {{{ Site
Site :: struct {
  // The base url the site will be accessible at. The string should preferably 
  // not end in a trailing slash.
  base_url: URL,

  // The root of the directory containing the source files to generate the
  // website out of.
  content_root: Path__Absolute,

  // The path to write the generated files to. Anything already present at said
  // path will get nuked from orbit.
  out_root: Path__Absolute,

  // Stores the source of all the files that have been read. Due to the design
  // of the parser/lexer, these sources need to be kept around, as various 
  // strings are allowed to point into them. In the future, I might change the 
  // code to clone the relevant strings, although that's not worth worrying
  // about for now.
  //
  // Other things stored here include the errors encountered along the way,
  // and the list of pages.
  forever_arena: virtual.Arena,

  statistics: Statistics,
  errors:     Exparr(Parsing_Error),
  pages:      Exparr(Page),
  files:      Exparr(File_Gen_Entry),
  xml:        Xml_Gen,
  codec_kit:  Codec_Kit,
  page_codec: Typed_Codec(Page),
  parser:     Parser,
}

site__make :: proc(site: ^Site, base_url, content_root, out_root: string) {
  log.assert(mem__iz(site^))

  if !os.exists(content_root) {
    fmt.eprintfln("Path %v not found", content_root)
    os.exit(1)
  }

  site.base_url     = URL(base_url)
  site.content_root = Path__Absolute(content_root)
  site.out_root     = Path__Absolute(out_root)

	err := virtual.arena_init_static(&site.forever_arena)
	log.assert(err == nil)

  forever := virtual.arena_allocator(&site.forever_arena)
  site.pages.allocator  = forever
  site.errors.allocator = forever
  site.files.allocator  = forever

	codec__kit__make(&site.codec_kit, &site.statistics)
	parser__make(&site.parser, &site.statistics)
  site.page_codec = codec__page(&site.codec_kit)
  xml__make(&site.xml, &site.statistics)
}

site__error :: proc(site: ^Site, loc: Error_Location, msg: string) {
	exparr__push(&site.errors, Parsing_Error{loc, msg})
}

site__errorf :: proc(
  site: ^Site, loc: Error_Location, format: string, args: ..any
) {
	allocator := virtual.arena_allocator(&site.forever_arena)
	msg := fmt.aprintf(format, ..args, allocator = allocator)
	site__error(site, loc, msg)
}

site__check_errors :: proc(site: ^Site) {
  if site.errors.len > 0 {
    for i in 0..<site.errors.len {
      err := exparr__get(site.errors, i)^
      fmt.eprintln(pretty_error(err))
    }

    os.exit(1)
  }
}
// }}}
// {{{ Path & url handling
URL :: distinct string
Path__Absolute :: distinct string
Path__Relative :: distinct string

site__path :: proc(site: ^Site, str: string) -> Path__Absolute {
  allocator := virtual.arena_allocator(&site.forever_arena)
  clone, err := strings.clone(str, allocator)
  log.assert(err == nil)
  return Path__Absolute(clone)
}

site__relative_path :: proc(
  site: ^Site, base, target: Path__Absolute
) -> Path__Relative {
  allocator := virtual.arena_allocator(&site.forever_arena)
  path, err := os.get_relative_path(string(base), string(target), allocator)
  log.assertf(err == nil, "Path %v cannot be made relative to %v", target, base)
  return Path__Relative(path)
}

// NOTE: we do not handle ".." segments
site__absolute_path :: proc(
  site: ^Site, base: Path__Absolute, target: Path__Relative
) -> Path__Absolute {
  if target == "." do return base
  allocator := virtual.arena_allocator(&site.forever_arena)
  str := fmt.aprintf("%v/%v", string(base), string(target), allocator=allocator)
  return Path__Absolute(str)
}

// NOTE: we do not handle ".." segments
site__url_at :: proc(site: ^Site, url: URL, path: Path__Relative) -> URL {
  if path == "." do return url
  allocator := virtual.arena_allocator(&site.forever_arena)
  str := fmt.aprintf("%v/%v", string(url), string(path), allocator=allocator)
  return URL(str)
}
// }}}
// {{{ XML
Xml_Gen :: struct {
  statistics:     ^Statistics,
  internal_arena: virtual.Arena,
  builder_arena:  virtual.Arena,
  output_arena:   virtual.Arena,
  builder:        strings.Builder,
  tag_stack:      Exparr(string),
  stage:          enum { Attributes, Content },
  single:         bool,
}

xml__make :: proc(gen: ^Xml_Gen, statistics: ^Statistics) {
  log.assert(mem__is_zero(gen^))
  gen.statistics = statistics
	err := virtual.arena_init_static(&gen.internal_arena)
	log.assert(err == nil)
	err = virtual.arena_init_static(&gen.output_arena)
	log.assert(err == nil)
	err = virtual.arena_init_static(&gen.builder_arena)
	log.assert(err == nil)
  builder_alloc := virtual.arena_allocator(&gen.builder_arena)
  strings.builder_init_none(&gen.builder, builder_alloc)
  gen.tag_stack.allocator = virtual.arena_allocator(&gen.internal_arena)
  gen.stage = .Content
}

xml__clear :: proc(gen: ^Xml_Gen) {
  strings.builder_reset(&gen.builder)
  gen.tag_stack = {}
  gen.tag_stack.allocator = virtual.arena_allocator(&gen.internal_arena)
  gen.stage = .Content
  virtual.arena_free_all(&gen.internal_arena)
  virtual.arena_free_all(&gen.builder_arena)
}

xml__save :: proc(gen: ^Xml_Gen) -> string {
  allocator := virtual.arena_allocator(&gen.output_arena)
  clone, err := strings.clone(strings.to_string(gen.builder), allocator)
  log.assert(err == nil)
  return clone
}

xml__ensure_content :: proc(gen: ^Xml_Gen) {
  log.assert(!gen.single)
  if gen.stage == .Attributes {
    fmt.sbprint(&gen.builder, ">")
    gen.stage = .Content
  }
}

xml__attr :: proc(gen: ^Xml_Gen, name: string, value: string) {
  gen.statistics.xml_attrs += 1
  // TODO: escaping
  log.assert(gen.stage == .Attributes)
  fmt.sbprintf(&gen.builder, " %v=\"%v\"", name, value)
}

xml__attrf :: proc(gen: ^Xml_Gen, name: string, fstr: string, args: ..any) {
  allocator := virtual.arena_allocator(&gen.internal_arena)
  xml__attr(gen, name, fmt.aprintf(fstr, ..args, allocator=allocator))
}

xml__raw_string :: proc(gen: ^Xml_Gen, value: string) {
  xml__ensure_content(gen)
  fmt.sbprint(&gen.builder, value)
}

xml__raw_stringf :: proc(gen: ^Xml_Gen, fstr: string, args: ..any) {
  xml__ensure_content(gen)
  fmt.sbprintf(&gen.builder, fstr, ..args)
}

xml__string :: proc(gen: ^Xml_Gen, value: string) {
  // TODO: escaping
  xml__raw_string(gen, value)
}

xml__stringf :: proc(gen: ^Xml_Gen, fstr: string, args: ..any) {
  // TODO: escaping
  xml__raw_stringf(gen, fstr, ..args)
}

xml__ctext :: proc(gen: ^Xml_Gen, ctext: Contiguous_Text) {
  allocator := virtual.arena_allocator(&gen.internal_arena)
  str := contiguous_text__concat(ctext, allocator)
  xml__string(gen, str)
}

// We return a boolean such that this can be used with if statements.
@(deferred_in=xml__tag_end)
xml__tag :: proc(
  gen: ^Xml_Gen, name: string, single: bool = false
) -> bool {
  gen.statistics.xml_tags += 1
  xml__ensure_content(gen)
  exparr__push(&gen.tag_stack, name)
  fmt.sbprintf(&gen.builder, "<%v", name)
  gen.stage = .Attributes
  return true
}

xml__tag_end :: proc(gen: ^Xml_Gen, name: string, single: bool) {
  last := exparr__pop(&gen.tag_stack)
  if gen.single {
    fmt.sbprintf(&gen.builder, "/>")
  } else {
    xml__ensure_content(gen)
    fmt.sbprintf(&gen.builder, "</%v>", last)
  }

  gen.single = false
}
// }}}
// {{{ File generation
File_Gen_Entry :: struct {
  path:     Path__Relative,
  contents: string,
}

// Note that this will not generate the physical file right away. Instead, one
// must call "site__commit" for that to take place.
site__add_file :: proc(site: ^Site, path: Path__Relative, str: string) {
  site.statistics.files_generated += 1
  entry := File_Gen_Entry { path, str }
  exparr__push(&site.files, entry)
}

// Commit the site's generated files to disk.
site__commit :: proc(site: ^Site) {
  // Nuke directory & contents if they exists
  if os.exists(string(site.out_root)) {
    // The following allocates, with no other way to override said allocation
    context.allocator = virtual.arena_allocator(&site.forever_arena)
    err := os.remove_all(string(site.out_root))
    if err != nil {
      site__errorf(site, site.out_root, "Failed to clean directory: %v", err)
      return
    }
  }

  // Create it again
  err := os.make_directory_all(string(site.out_root))
  if err != nil {
    site__errorf(site, site.out_root, "Failed to create directory: %v", err)
    return
  }

  for i in 0..<site.files.len {
    entry := exparr__get(site.files, i)

    full_path := site__absolute_path(site, site.out_root, entry.path)
    dir, _ := os.split_path(string(full_path))

    if !os.exists(dir) {
      err := os.make_directory_all(dir)
      if err != nil {
        dir := Path__Absolute(dir)
        site__errorf(site, dir, "Failed to create directory: %v", err)
        continue
      }
    }

    if os.exists(string(full_path)) {
      site__errorf(site, full_path, "File already exists")
      continue
    }

    err = os.write_entire_file_from_string(string(full_path), entry.contents)
    if err != nil {
      site__errorf(site, full_path, "Failed to write file: %v", err)
      continue
    }
  }
}
// }}}
// {{{ Sitemap
site__sitemap :: proc(site: ^Site) -> string {
  g := &site.xml
  defer xml__clear(g)

  xml__raw_string(
    g,
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  )

  if xml__tag(g, "urlset") {
    xml__attr(g, "xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")
    for i in 0..<site.pages.len {
      page := exparr__get(site.pages, i)
      page.public or_continue
      xml__tag(g, "url")

      if xml__tag(g, "loc") {
        xml__stringf(g, "%v", site__url_at(site, site.base_url, page.site_path))
      }

      if xml__tag(g, "last_mod") {
        allocator := virtual.arena_allocator(&g.internal_arena)
        at := page__last_updated(page^)
        str, ok := time.time_to_rfc3339(at, allocator=allocator)
        log.assert(ok)
        xml__stringf(g, "%v", str)
      }

      if mem__non_zero(page.priority) {
        if xml__tag(g, "priority") do xml__ctext(g, page.priority)
      }

      if mem__non_zero(page.changefreq) {
        if xml__tag(g, "changefreq") do xml__ctext(g, page.changefreq)
      }
    }
  }

  return xml__save(g)
}
// }}}
// {{{ RSS feed
site__feed :: proc(
  site: ^Site, base: Page, feed: Def__Feed
) -> (path: Path__Relative, content: string) {
  g := &site.xml
  defer xml__clear(g)

  xml__raw_string(
    g,
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  )

  forever := virtual.arena_allocator(&site.forever_arena)
  feed_path_str := fmt.aprintf("%v.xml", feed.at, allocator=forever)
  feed_path := Path__Relative(feed_path_str)
  feed_url := site__url_at(site, site.base_url, feed_path)

  if xml__tag(g, "rss") {
    xml__attr(g, "version", "2.0")
    if xml__tag(g, "channel") {
      xml__attr(g, "xmlns:atom", "http://www.w3.org/2005/Atom")
      
      // TODO: markup -> text
      if xml__tag(g, "title") do xml__stringf(g, "Moonythm | %v", "feed.name")
      if xml__tag(g, "link") do xml__stringf(g, "%v", site.base_url)

      // TODO: markup -> text
      if xml__tag(g, "description") do xml__stringf(g, "%v", "feed.description")
      if xml__tag(g, "language") do xml__string(g, "en")
      if xml__tag(g, "generator") do xml__string(g, "anima")
      if xml__tag(g, "webMaster") {
        xml__string(g, "hi@moonythm.dev (prescientmoon)")
      }

      if xml__tag(g, "atom:link", true) {
        xml__attrf(g, "href", "%v", feed_url)
        xml__attr(g, "rel", "self")
        xml__attr(g, "type", "application/rss+xml")
      }

      last_update: time.Time
      for i in 0..<site.pages.len {
        page := exparr__get(site.pages, i)
        page_filter__all__eval(base, page^, feed.members) or_continue

        last_update = time__max(last_update, page__last_updated(page^))

        xml__tag(g, "item")
        if xml__tag(g, "author") {
          xml__string(g, "hi@moonythm.dev (prescientmoon)")
        }

        title := page__title(page^)
        if xml__tag(g, "title") {
          // TODO: markup -> text
          if mem__is_zero(title) do xml__string(g, "???")
          else do xml__stringf(g, "%v", "title.content")
        }

        if xml__tag(g, "description") {
          // TODO: markup -> text
          // TODO: what do we do when there's no description?
          xml__stringf(g, "%v", "page.description")
        }

        if page.published_at != {} {
          if xml__tag(g, "pubDate") {
            xml__stringf(g, "%v", Rfc2822(page.published_at))
          }
        }

        url := site__url_at(site, site.base_url, page.site_path)
        if xml__tag(g, "link") do xml__stringf(g, "%v", url)
        if xml__tag(g, "guid") {
          xml__attr(g, "isPermaLink", "true")
          xml__stringf(g, "%v", url)
        }
      }

      if last_update != {} {
        if xml__tag(g, "lastBuildDate") {
          xml__stringf(g, "%v", Rfc2822(last_update))
        }
      }
    }
  }

  return feed_path, xml__save(g)
}
// }}}
// {{{ Path collection
site__collect :: proc(site: ^Site) {
  collect_under :: proc(site: ^Site, directory: Path__Absolute) {
    site.statistics.directories_visited += 1
    file, err := os.open(string(directory))

    if err != nil {
      site__errorf(site, directory, "Failed to read directory: %v", err)
      return
    }

    iter := os.read_directory_iterator_create(file)
    defer os.read_directory_iterator_destroy(&iter)

    for info in os.read_directory_iterator(&iter) {
      if spath, err := os.read_directory_iterator_error(&iter); err != nil {
        path := site__path(site, spath)
        site__errorf(site, path, "Failed to read file: %v", err)
        continue
      }

      path := site__path(site, info.fullpath)

      // NOTE: we do not handle symlinks
      #partial switch info.type {
      case .Directory:
        collect_under(site, path)
      case .Regular: 
        ext := os.base(info.fullpath)
        if ext == "page.anima" {
          allocator := virtual.arena_allocator(&site.forever_arena)
          bytes, err := os.read_entire_file_from_path(info.fullpath, allocator)

          if err != nil {
            site__errorf(site, path, "Failed to read page: %v", err)
            continue
          }

          site.statistics.pages += 1
          page: ^Page

          file := new(File, allocator)
          file.source = string(bytes)

          fullpath, clone_err := strings.clone(info.fullpath, allocator)
          log.assert(clone_err == nil)
          file.name = fullpath

          if parser__lex(&site.parser, file) {
            page = exparr__push(&site.pages, Page{})
            parser__eval(&site.parser, site.page_codec, page)
          }

          if site.parser.errors.len > 0 {
            exparr__push_exparr(&site.errors, site.parser.errors)
          } else {
            page.source_path = path
            page.site_path = site__relative_path(
              site,
              site.content_root,
              directory
            )
          }

          parser__clear(&site.parser)
        }
      }
    }

    if spath, err := os.read_directory_iterator_error(&iter); err != nil {
      path := Path__Absolute(spath)
      site__errorf(site, path, "Failed to read directory: %v", err)
    }
  }

  collect_under(site, site.content_root)
}
// }}}
// {{{ Site geenration
site__generate :: proc(site: ^Site) {
  site__add_file(site, Path__Relative("sitemap.xml"), site__sitemap(site))
  for i in 0..<site.pages.len {
    page := exparr__get(site.pages, i)
    for j in 0..<page.feeds.len {
      feed := exparr__get(page.feeds, j)
      feed_path, feed_content := site__feed(site, page^, feed^)
      site__add_file(site, feed_path, feed_content)
    }
  }
}
// }}}
