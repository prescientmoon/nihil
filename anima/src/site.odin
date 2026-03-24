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

  // A dynamic-stack of sorts the various systems can use for internal data.
  stack_arena:   virtual.Arena,

  // Fun struct keeping track of how many times we've done various things.
  statistics: Statistics,

  // All the subsystems push their error data here.
  errors:     Exparr(Error),

  // The result of parsing the various files.
  pages:      Exparr(Page),

  // Cool URLs never change!
  redirects:  Exparr(Redirect),

  // The output file data, to be generated in one go.
  files:      Exparr(File_Gen_Entry),

  // Used to generate XML for the various files.
  xml:        Xml_Gen,

  // The main codec used to parse pages.
  page_codec: Typed_Codec(Page),
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

	err = virtual.arena_init_static(&site.stack_arena)
	log.assert(err == nil)

  forever := virtual.arena_allocator(&site.forever_arena)
  site.pages.allocator     = forever
  site.redirects.allocator = forever
  site.errors.allocator    = forever
  site.files.allocator     = forever

	codec_kit := codec__kit__make(site)
  site.page_codec = codec__page(&codec_kit)
	codec__kit__destroy(codec_kit)

  xml__make(&site.xml, &site.statistics)
}

site__destroy :: proc(site: ^Site) {
  xml__destroy(&site.xml)
  arena__destroy(&site.statistics.site_forever_arena, &site.forever_arena)
  arena__destroy(&site.statistics.site_stack_arena, &site.stack_arena)
}
// }}}
// {{{ Error reporting
site__error :: proc(site: ^Site, loc: Error_Location, msg: string) {
	exparr__push(&site.errors, Error{loc, msg})
}

site__errorf :: proc(
  site: ^Site, loc: Error_Location, format: string, args: ..any
) {
	msg := fmt.aprintf(format, ..args, allocator = site__alloc(site))
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
// {{{ Allocators
Site_Alloc :: enum { 
  Forever, // Stuff allocated on here will last as long as the site struct
  Stack,   // A dynamic stack arena
}

site__alloc :: proc(site: ^Site, alloc: Site_Alloc = .Forever) -> mem.Allocator {
  switch alloc {
  case .Forever: return virtual.arena_allocator(&site.forever_arena)
  case .Stack:   return virtual.arena_allocator(&site.stack_arena)
  }

  log.panic("Impossible")
}

// Begin a dynamic stack frame
@(deferred_in_out=site__frame_end)
site__frame :: proc(site: ^Site, ignored := false) -> virtual.Arena_Temp {
  return virtual.arena_temp_begin(&site.stack_arena)
}

@(private="file")
site__frame_end :: proc(site: ^Site, ignored: bool, temp: virtual.Arena_Temp) {
  site__update_stack_stats(site)
  if ignored {
    virtual.arena_temp_ignore(temp)
  } else {
    virtual.arena_temp_end(temp)
  }
}

// Since the code uses dynamic stacks, the usual arena stat tracking no longer
// works (since the arena is always zeroed at the beginning/end). Instead, we
// manually call this at key points.
site__update_stack_stats :: proc(site: ^Site) {
  size := &site.statistics.site_stack_arena
  size^ = max(size^, Bytes(site.stack_arena.total_used))
}
// }}}
// {{{ Checking 
site__check :: proc(site: ^Site) {
  for i in 0..<site.pages.len {
    page := exparr__get(site.pages, i)
    page__check(site, page)
  }

  // This is inefficient as fuck, but it's fineeee
  // {{{ Monstrous link/icon duplicate checker
  for i in 0..<site.pages.len {
    defsite1 := exparr__get(site.pages, i)
    for j in i..<site.pages.len {
      defsite2 := exparr__get(site.pages, j)
      for l in 0..<defsite1.links.len {
        link1 := exparr__get(defsite1.links, l)
        for m in 0..<defsite2.links.len {
          link2 := exparr__get(defsite2.links, m)

          (link1.id == link2.id)          or_continue
          (link1 != link2)                or_continue
          (defsite1 != defsite2 || m > l) or_continue

          for k in 0..<site.pages.len {
            test_page := exparr__get(site.pages, k)

            page_filter__eval(defsite1^, test_page^, link1.scope) or_continue
            page_filter__eval(defsite2^, test_page^, link2.scope) or_continue

            site__errorf(
              site,
              test_page.source_path,
              "Conflicting definitions for link %v in scope. " \
              + "The two definitions reside at %v and %v repectively.",
              link1.id,
              defsite1.source_path,
              defsite2.source_path,
            )

            break
          }
        }
      }
    }
  }

  for i in 0..<site.pages.len {
    defsite1 := exparr__get(site.pages, i)
    for j in i..<site.pages.len {
      defsite2 := exparr__get(site.pages, j)
      for l in 0..<defsite1.icons.len {
        icon1 := exparr__get(defsite1.icons, l)
        for m in 0..<defsite2.icons.len {
          icon2 := exparr__get(defsite2.icons, l)

          (icon1.id == icon2.id)          or_continue
          (icon1 != icon2)                or_continue
          (defsite1 != defsite2 || m > l) or_continue

          for k in 0..<site.pages.len {
            test_page := exparr__get(site.pages, k)

            page_filter__eval(defsite1^, test_page^, icon1.scope) or_continue
            page_filter__eval(defsite2^, test_page^, icon2.scope) or_continue

            site__errorf(
              site,
              test_page.source_path,
              "Conflicting definitions for icon %v in scope. " \
              + "The two definitions reside at %v and %v repectively.",
              icon1.id,
              defsite1.source_path,
              defsite2.source_path,
            )

            break
          }
        }
      }
    }
  }
  // }}}
}
// }}}

// Smaller components, I guess
// {{{ Path & url handling
URL :: distinct string
Path :: distinct string // Considered absolute if and only if it starts with /
Path__Absolute :: distinct string
Path__Relative :: distinct string
Path__Input :: distinct Path__Relative // A path relative to the content root
Path__Output :: distinct Path__Relative // A path reliative to the output root

@(private = "file")
site__relative :: proc(
  site: ^Site, base, target: Path__Absolute
) -> Path__Relative {
  allocator := site__alloc(site)
  path, err := os.get_relative_path(string(base), string(target), allocator)
  log.assertf(err == nil, "Path %v cannot be made relative to %v", target, base)
  return Path__Relative(path)
}

@(private = "file")
site__ipath :: proc(
  site: ^Site, target: Path__Absolute
) -> Path__Input {
  return Path__Input(site__relative(site, site.content_root, target))
}

// NOTE: we do not handle ".." segments
@(private = "file")
site__absolute :: proc(
  site: ^Site,
  base: Path__Absolute,
  target: $T/Path__Relative,
  alloc: Site_Alloc = .Forever,
) -> Path__Absolute {
  if target == "." do return base
  str := fmt.aprintf("%v/%v", base, target, allocator=site__alloc(site, alloc))
  return Path__Absolute(str)
}

// NOTE: we do not handle ".." segments
site__resolve :: proc(
  site: ^Site,
  base: $T/Path__Relative,
  target: Path,
  alloc: Site_Alloc = .Forever,
) -> T {
  // Unicode does not exist and cannot hurt me
  // NOTE: I have no idea if this works on Windows
  switch {
  case len(target) > 0 && target[0] == '/':
    return T(target[1:])
  case base == ".": 
    return T(target)
  case target == ".": 
    return base
  case:
    str := fmt.aprintf("%v/%v", base, target, allocator=site__alloc(site, alloc))
    return T(str)
  }
}

// NOTE: we do not handle ".." segments
site__url :: proc(
  site: ^Site, path: Path__Output, alloc: Site_Alloc = .Stack
) -> URL {
  if path == "." do return site.base_url
  alloc := site__alloc(site, alloc)
  return URL(fmt.aprintf("%v/%v", site.base_url, path, allocator=alloc))
}
// }}}
// {{{ Virtual-arena based string builders
// I used to use this in more than one place, but I no longer do. I might remove
// this in the future (or well, simply inline its definition into Xml_Gen.
Virtual_String_Builder :: struct {
  using builder: strings.Builder,
  arena:   virtual.Arena,
}

vsb__make :: proc(vsb: ^Virtual_String_Builder) {
  log.assert(mem__is_zero(vsb^))
	err := virtual.arena_init_static(&vsb.arena)
	log.assert(err == nil)
  builder_alloc := virtual.arena_allocator(&vsb.arena)
  strings.builder_init_none(&vsb.builder, builder_alloc)
}

vsb__destroy :: proc(bytes: ^Bytes, vsb: ^Virtual_String_Builder) {
	arena__destroy(bytes, &vsb.arena)
}

vsb__clear :: proc(bytes: ^Bytes, vsb: ^Virtual_String_Builder) {
  strings.builder_reset(&vsb.builder)
  arena__clear(bytes, &vsb.arena)
}
// }}}
// {{{ XML building
// A builder that can be used to construct XML output in-order. In particular,
// one must generate the attributes before the content for any given tag.
Xml_Gen :: struct {
  statistics:     ^Statistics,
  internal_arena: virtual.Arena, // Junk that needs not survive a clear
  builder:        Virtual_String_Builder,
  tag_stack:      Exparr(string),
  stage:          enum { Attributes, Content },
  single:         bool,
}

xml__make :: proc(gen: ^Xml_Gen, statistics: ^Statistics) {
  log.assert(mem__is_zero(gen^))
  gen.statistics = statistics
	err := virtual.arena_init_static(&gen.internal_arena)
	log.assert(err == nil)
  gen.tag_stack.allocator = virtual.arena_allocator(&gen.internal_arena)
  gen.stage = .Content
  vsb__make(&gen.builder)
}

xml__destroy :: proc(gen: ^Xml_Gen) {
	arena__destroy(&gen.statistics.xml_internal_arena, &gen.internal_arena)
	vsb__destroy(&gen.statistics.xml_builder_arena, &gen.builder)
}

xml__clear :: proc(gen: ^Xml_Gen) {
  gen.tag_stack = {}
  gen.tag_stack.allocator = virtual.arena_allocator(&gen.internal_arena)
  gen.stage = .Content
  arena__clear(&gen.statistics.xml_internal_arena, &gen.internal_arena)
  vsb__clear(&gen.statistics.xml_builder_arena, &gen.builder)
}

xml__ensure_content :: proc(gen: ^Xml_Gen) {
  log.assert(!gen.single)
  if gen.stage == .Attributes {
    fmt.sbprint(&gen.builder, ">")
    gen.stage = .Content
  }
}

// An attribute without an associated value
xml__flag :: proc(gen: ^Xml_Gen, name: string) {
  gen.statistics.xml_attrs += 1
  log.assert(gen.stage == .Attributes)
  fmt.sbprintf(&gen.builder, " %v", name)
}

// NOTE: always run this inside a temporary block
xml__escape :: proc(gen: ^Xml_Gen, formatted: string) -> string {
  temp_alloc := virtual.arena_allocator(&gen.internal_arena)

  builder: strings.Builder
  strings.builder_init_none(&builder, temp_alloc)

  needs_escaping := false
  for char in formatted {
    if char == '&' || char == '<' || char == '>' || char == '"' {
      needs_escaping = true
      break
    }
  }

  if needs_escaping {
    unescaped, err := strings.clone(formatted, temp_alloc)
    log.assert(err == nil)
    strings.builder_reset(&builder)

    for char in unescaped {
      switch char {
      case '&': strings.write_string(&builder, "&amp;")
      case '"': strings.write_string(&builder, "&quot;")
      case '<': strings.write_string(&builder, "&lt;")
      case '>': strings.write_string(&builder, "&gt;")
      case: strings.write_rune(&builder, char)
      }
    }

    return strings.to_string(builder)
  } else {
    return formatted
  }
}

xml__attr :: proc(gen: ^Xml_Gen, name: string, value: any) {
  xml__attrf(gen, name, "%v", value)
}

xml__attrf :: proc(gen: ^Xml_Gen, name: string, fstr: string, args: ..any) {
  gen.statistics.xml_attrs += 1
  log.assert(gen.stage == .Attributes)

  temp_alloc := virtual.arena_allocator(&gen.internal_arena)
  temp := virtual.arena_temp_begin(&gen.internal_arena)
  defer virtual.arena_temp_end(temp)

  builder: strings.Builder
  strings.builder_init_none(&builder, temp_alloc)
  fmt.sbprintf(&builder, fstr, ..args)
  escaped := xml__escape(gen, strings.to_string(builder))
  fmt.sbprintf(&gen.builder, " %v=\"%v\"", name, escaped)
}

xml__raw_string :: proc(gen: ^Xml_Gen, value: string) {
  xml__ensure_content(gen)
  fmt.sbprint(&gen.builder, value)
}

xml__raw_stringf :: proc(gen: ^Xml_Gen, fstr: string, args: ..any) {
  xml__ensure_content(gen)
  fmt.sbprintf(&gen.builder, fstr, ..args)
}

xml__string :: proc(gen: ^Xml_Gen, value: any) {
  xml__stringf(gen, "%v", value)
}

xml__stringf :: proc(gen: ^Xml_Gen, fstr: string, args: ..any) {
  temp_alloc := virtual.arena_allocator(&gen.internal_arena)
  temp := virtual.arena_temp_begin(&gen.internal_arena)
  defer virtual.arena_temp_end(temp)
  // defer site__update_stack_stats(instance.site)

  builder: strings.Builder
  strings.builder_init_none(&builder, temp_alloc)
  fmt.sbprintf(&builder, fstr, ..args)
  xml__raw_string(gen, xml__escape(gen, strings.to_string(builder)))
}

// We return a boolean such that this can be used with if statements.
@(deferred_in=xml__tag_end_auto)
xml__tag :: proc(
  gen: ^Xml_Gen, name: string, single := false, auto_close := true
) -> bool {
  log.assert(name != {})
  log.assert(!single || auto_close) // single implies auto_close
  gen.statistics.xml_tags += 1
  xml__ensure_content(gen)
  exparr__push(&gen.tag_stack, name)
  fmt.sbprintf(&gen.builder, "<%v", name)
  gen.stage = .Attributes
  gen.single = single
  return true
}

xml__tag_end_auto :: proc(
  gen: ^Xml_Gen, name: string, single, auto_close: bool
) {
  if !auto_close do return
  xml__tag_end(gen)
}

xml__tag_end :: proc(gen: ^Xml_Gen) {
  last := exparr__pop(&gen.tag_stack)
  if gen.single {
    gen.stage = .Content
    fmt.sbprintf(&gen.builder, "/>")
  } else {
    xml__ensure_content(gen)
    fmt.sbprintf(&gen.builder, "</%v>", last)
  }

  gen.single = false
}

@(private="file")
site__xml :: proc(site: ^Site) -> string {
  allocator := site__alloc(site)
  clone, err := strings.clone(strings.to_string(site.xml.builder), allocator)
  log.assert(err == nil)
  return clone
}
// }}}
// {{{ Path collection
// This function recursively looks for page.anima files, parsing them and saving
// the results in the given site struct. The process will not stop on the first
// error.
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
        path := site__ipath(site, Path__Absolute(spath))
        site__errorf(site, path, "Failed to read file: %v", err)
        continue
      }

      path := Path__Absolute(info.fullpath)

      // NOTE: we do not handle symlinks
      #partial switch info.type {
      case .Directory:
        collect_under(site, path)
      case .Regular: 
        ext := os.base(info.fullpath)
        if ext == "page.anima" {
          allocator := site__alloc(site)
          bytes, err := os.read_entire_file_from_path(info.fullpath, allocator)

          if err != nil {
            ipath := site__ipath(site, path)
            site__errorf(site, ipath, "Failed to read page: %v", err)
            continue
          }

          site.statistics.pages += 1

          file := new(File, allocator)
          file.source = string(bytes)

          fullpath, clone_err := strings.clone(info.fullpath, allocator)
          log.assert(clone_err == nil)
          file.path = site__ipath(site, Path__Absolute(info.fullpath))

          page: Page
          if parser__eval(site, site.page_codec, file, &page) {
            page.source_path = site__ipath(site, directory)
            exparr__push(&site.pages, page)
          }
        }
      }
    }

    if spath, err := os.read_directory_iterator_error(&iter); err != nil {
      ipath := site__ipath(site, Path__Absolute(spath))
      site__errorf(site, ipath, "Failed to read directory: %v", err)
    }
  }

  collect_under(site, site.content_root)
}
// }}}
// {{{ Statistics
// A wrapper around unsigned integers, which prints the integer as a number of 
// bytes in human readable format (i.e. with a B/KB/MB/etc prefix).
Bytes :: distinct uint

Statistics :: struct {
	tokens:                    uint,
	codecs:                    uint,
	codec_evaluations:         uint,
  xml_tags:                  uint,
  xml_attrs:                 uint,
  pages:                     uint,
  directories_visited:       uint,
  files_generated:           uint,

  system_arena:          Bytes,
  site_forever_arena:    Bytes,
  site_stack_arena:      Bytes,
  xml_internal_arena:    Bytes,
  xml_builder_arena:     Bytes,
}

// Destroy an arena, saving the stats of how much memory it used up
arena__clear :: proc(bytes: ^Bytes, arena: ^virtual.Arena) {
  bytes^ = max(Bytes(arena.total_used), bytes^)
  virtual.arena_free_all(arena)
}

// Destroy an arena, saving the stats of how much memory it used up
arena__destroy :: proc(bytes: ^Bytes, arena: ^virtual.Arena)  {
  bytes^ = max(Bytes(arena.total_used), bytes^)
  virtual.arena_destroy(arena)
}
// }}}

// Generation
// {{{ Sitemap
@(private = "file")
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
        xml__stringf(g, "%v", page.url)
      }

      if xml__tag(g, "last_mod") {
        allocator := virtual.arena_allocator(&g.internal_arena)
        at := page__last_updated(page^)
        str, ok := time.time_to_rfc3339(at, allocator=allocator)
        log.assert(ok)
        xml__stringf(g, "%v", str)
      }

      if mem__non_zero(page.priority) {
        if xml__tag(g, "priority") do xml__string(g, page.priority)
      }

      if mem__non_zero(page.changefreq) {
        if xml__tag(g, "changefreq") do xml__string(g, page.changefreq)
      }
    }
  }

  return site__xml(site)
}
// }}}
// {{{ RSS feed
@(private = "file")
site__feed :: proc(
  site: ^Site, base: Page, feed: Def__Feed
) -> (path: Path__Output, content: string) {
  g := &site.xml
  defer xml__clear(g)

  xml__raw_string(
    g,
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  )

  if xml__tag(g, "rss") {
    xml__attr(g, "version", "2.0")
    if xml__tag(g, "channel") {
      xml__attr(g, "xmlns:atom", "http://www.w3.org/2005/Atom")
      
      name := strings.trim_space(feed.name)
      if xml__tag(g, "title") do xml__stringf(g, "Moonythm | %v", name)
      if xml__tag(g, "link") do xml__string(g, site.base_url)

      description := strings.trim_space(feed.description)
      if xml__tag(g, "description") do xml__string(g, description)
      if xml__tag(g, "language") do xml__string(g, "en")
      if xml__tag(g, "generator") do xml__string(g, GENERATOR)
      if xml__tag(g, "webMaster") {
        xml__stringf(g, "%v %v", EMAIL, USERNAME)
      }

      if xml__tag(g, "atom:link", true) {
        xml__attr(g, "href", site__url(site, feed.site_path, .Stack))
        xml__attr(g, "rel", "self")
        xml__attr(g, "type", "application/rss+xml")
      }

      last_update: time.Time
      for iter := iter__mk(site.pages); page in iter__next(&iter) {
        page_filter__all__eval(base, page^, feed.members) or_continue

        last_update = time__max(last_update, page__last_updated(page^))

        xml__tag(g, "item")
        if xml__tag(g, "author") {
          xml__stringf(g, "%v %v", EMAIL, USERNAME)
        }

        if xml__tag(g, "title") {
          xml__string(g, inline_markup__formatter(site, page, &page.title))
        }

        if xml__tag(g, "description") {
          if mem__iz(page.description) {
            xml__string(g, ":3")
          } else  {
            xml__string(
              g,
              inline_markup__formatter(site, page, &page.description)
            )
          }
        }

        if page.published_at != {} {
          if xml__tag(g, "pubDate") {
            xml__string(g, Rfc2822(page.published_at))
          }
        }

        if xml__tag(g, "link") do xml__string(g, page.url)
        if xml__tag(g, "guid") {
          xml__attr(g, "isPermaLink", "true")
          xml__string(g, page__guid(site, page^, .Stack))
        }

        for j in 0..<page.tags.len {
          tag := exparr__get(page.tags, j)^
          xml__tag(g, "category")
          xml__string(g, tag)
        }
      }

      if last_update != {} {
        if xml__tag(g, "lastBuildDate") {
          xml__string(g, Rfc2822(last_update))
        }
      }
    }
  }

  return feed.site_path, site__xml(site)
}
// }}}
// {{{ Files
@(private = "file")
File_Gen_Content :: union {
  string,
  Path__Input,
}

@(private = "file")
File_Gen_Entry :: struct {
  path:    Path__Output,
  content: File_Gen_Content,
}

// Note that this will not generate the physical file right away. Instead, one
// must call "site__commit" for that to take place.
@(private = "file")
site__add_file :: proc(
  site: ^Site, path: Path__Output, content: File_Gen_Content
) {
  site.statistics.files_generated += 1
  entry := File_Gen_Entry { path, content }
  exparr__push(&site.files, entry)
}

// Commit the site's generated files to disk.
site__commit :: proc(site: ^Site) {
  site__frame(site)

  // Nuke directory & contents if they exists
  if os.exists(string(site.out_root)) {
    // The following allocates, with no other way to override said allocation
    context.allocator = site__alloc(site, .Stack)
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

  for iter := iter__mk(site.files); entry in iter__next(&iter) {
    site__frame(site)

    full_path := site__absolute(site, site.out_root, entry.path, .Stack)
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

    switch inner in entry.content {
    case string:
      err = os.write_entire_file_from_string(string(full_path), inner)
      if err != nil {
        site__errorf(site, full_path, "Failed to write file: %v", err)
        continue
      }
    case Path__Input:
      path := site__absolute(site, site.content_root, inner, .Stack)
      err = os.copy_file(string(full_path), string(path))
      if err != nil {
        site__errorf(site, full_path, "Failed to copy file: %v", err)
        continue
      }
    }
  }
}
// }}}
// {{{ Site
site__generate :: proc(site: ^Site) {
  forever := site__alloc(site)
  site__add_file(site, Path__Output("sitemap.xml"), site__sitemap(site))
  for iter := iter__mk(site.pages); page in iter__next(&iter) {
    for iter := iter__mk(page.feeds); feed in iter__next(&iter) {
      feed_path, feed_content := site__feed(site, page^, feed^)
      site__add_file(site, feed_path, feed_content)
    }

    page_path := site__resolve(site, page.site_path, "index.html")

    {
      defer xml__clear(&site.xml)
      page__html(site, page, .Self)
      site__add_file(site, page_path, site__xml(site))
    }

    for iter := iter__mk(page.styles); style in iter__next(&iter) {
      in_path := site__resolve(site, page.source_path, style.at)
      site__add_file(site, style.site_path, in_path)
    }
  }

  // Add guard pages to redirects, thus erroring out on path conflicts.
  for iter := iter__mk(site.redirects); redirect in iter__next(&iter) {
    site__add_file(
      site,
      site__resolve(site, redirect.from, "index.html"),
      fmt.aprintf(
        "This page has moved to <a href=\"%[0]v\">%[0]v</a>. " + \
        "You were supposed to get redirected there, but I guess " + \
        "something went wrong in the process. Consider clicking said " + \
        "link manually :3",
        site__url(site, redirect.to),
        allocator = forever
      )
    )
  }
}
// }}}
