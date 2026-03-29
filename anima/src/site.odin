// This file is the only one making use of the "core:os" module. The only code
// here that is NOT cross platform is the path handling, since we assume paths
// are "well-behaved", including the use of forward slashes. Making the code
// cross-platform wouldn't be *super* difficult in the grand scheme of things,
// although I have no reason to bother doing so right now (nor would I really
// have a way to test anyways).
package anima

import "core:os"
import "core:fmt"
import "core:mem"
import "core:log"
import "core:time"
import "core:strings"
import "core:mem/virtual"
import "core:path/slashpath"

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

  // The site's icon!
  favicon:    ^Def__Icon,

  // The output file data, to be generated in one go.
  files:      Exparr(File_Gen_Entry),

  // The main codec used to parse pages.
  page_codec: ^Codec,
}

site__make :: proc(site: ^Site, base_url, content_root, out_root: string) {
  log.assert(mem__is_zero(site^))

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

  {
    site__frame(site)
    codec_kit := codec__kit__make(site)
    site.page_codec = codec__page(&codec_kit)
  }
}

site__destroy :: proc(site: ^Site) {
  arena__destroy(&site.statistics.site_forever_arena, &site.forever_arena)
  arena__destroy(&site.statistics.site_stack_arena, &site.stack_arena)
}
// }}}
// {{{ Error reporting
site__error :: proc(site: ^Site, loc: Error_Location, msg: string) {
	push(&site.errors, Error{loc, msg})
}

site__errorf :: proc(
  site: ^Site, loc: Error_Location, format: string, args: ..any
) {
	msg := fmt.aprintf(format, ..args, allocator = site__alloc(site))
	site__error(site, loc, msg)
}

site__check_errors :: proc(site: ^Site) {
  if site.errors.len > 0 {
    for iter := iter__mk(site.errors); err in iter__next(&iter) {
      fmt.eprintln(pretty_error(err^))
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
  for iter := iter__mk(site.pages); page in iter__next(&iter) {
    page__check(site, page)
  }
}
// }}}

// Smaller components, I guess
// {{{ Path & url handling
URL :: distinct string
// Slash-separated paths. Considered absolute if and only starting with a /.
Path :: distinct string

Path__Relative :: distinct Path // A path that cannot start with a /.
Path__Absolute :: distinct string // An absolute OS path

// Note that the following paths are free to start with slashes (they're
// considered relative nonetheless).
Path__Input  :: distinct Path // A path relative to the content root
Path__Output :: distinct Path // A path reliative to the output root

// NOTE: this is *not* cross-platform.
@(private = "file")
site__relative :: proc(
  site: ^Site, base, target: Path__Absolute
) -> Path__Relative {
  allocator := site__alloc(site)
  path, err := os.get_relative_path(string(base), string(target), allocator)
  log.assertf(err == nil, "Path %v cannot be made relative to %v", target, base)
  // NOTE: this cast is not correct on all platforms!
  return Path__Relative(path)
}

// NOTE: this is *not* cross-platform.
@(private = "file")
site__absolute :: proc(
  site: ^Site,
  base: Path__Absolute,
  target: $T/Path,
  alloc: Site_Alloc = .Forever,
) -> Path__Absolute {
  if target == "." do return base
  str := fmt.aprintf("%v/%v", base, target, allocator=site__alloc(site, alloc))
  return Path__Absolute(str)
}

@(private = "file")
site__ipath :: proc(
  site: ^Site, target: Path__Absolute
) -> Path__Input {
  return Path__Input(site__relative(site, site.content_root, target))
}

site__resolve :: proc(
  site: ^Site,
  base: $T/Path__Relative,
  target: Path,
  alloc: Site_Alloc = .Forever,
) -> T {
  switch {
  case slashpath.is_abs(string(target)):
    return T(target)
  case:
    return T(slashpath.join(
      {string(base), string(target)},
      site__alloc(site, alloc),
    ))
  }
}

// NOTE: we do not handle ".." segments
site__url :: proc(
  site: ^Site, path: Path__Output, alloc: Site_Alloc = .Stack
) -> URL {
  alloc := site__alloc(site, alloc)
  relative := slashpath.join({".", string(path)}, site__alloc(site, .Stack))
  return URL(fmt.aprintf("%v/%v", site.base_url, relative, allocator=alloc))
}
// }}}
// {{{ XML building
// Files cannot exceed this size. Will be bumped once the size is reached in
// practice.
//
// We do this in order to simplify the memory management, since this means we
// don't need to worry about the string builder potentially growing while we're
// taking care of other things.
//
// Thins might feel a bit strange at first, but anima is not meant to be a
// general purpose static site generator, hence we are free to make use of
// specific facts about its real life usage.
//
// Note that another way to not have to worry about this would be to stream the
// XML dirrectly into a file, although that would require a few changes to the
// way we organise things.
MAX_XML_CONTENT_SIZE :: 16 * mem.Kilobyte

// A builder that can be used to construct XML output in-order. In particular,
// one must generate the attributes before the content for any given tag.
Xml_Gen :: struct {
  site:    ^Site,

  // The output we're generating is being written here.
  builder: strings.Builder,

  // What are we generating right now?
  stage:   enum { Attributes, Content },

  // Are we inside a self-closing tag? (i.e. <img ... />)
  single:  bool,
}

// Allocates data in the stack array.
// NOTE: We return a pointer since this will get passed around everywhere, and
// not having to take the ref in some places (and not take it in others)
// simplifies things a lot.
xml__make :: proc(site: ^Site) -> ^Xml_Gen {
  alloc := site__alloc(site, .Stack)
  gen := Xml_Gen {
    site = site,
    stage = .Content,
    builder = strings__fixed_builder(MAX_XML_CONTENT_SIZE, alloc),
  }

  return new_clone(gen, alloc)
}

@(private="file")
xml__ensure_content :: proc(gen: ^Xml_Gen) {
  log.assert(!gen.single)
  if gen.stage == .Attributes {
    fmt.sbprint(&gen.builder, ">")
    gen.stage = .Content
  }
}

// An attribute without an associated value
xml__flag :: proc(gen: ^Xml_Gen, name: string) {
  gen.site.statistics.xml_attrs += 1
  log.assert(gen.stage == .Attributes)
  fmt.sbprintf(&gen.builder, " %v", name)
}

// Escapes special character found in an XML string. In particular, this will
// take care of the following: <, >, ", and &.
// NOTE: Allocates the output string on the temporary stack.
@(private="file")
xml__escape :: proc(gen: ^Xml_Gen, fstr: string, args: ..any) -> string {
  builder := strings.builder_make_none(site__alloc(gen.site, .Stack))
  fmt.sbprintf(&builder, fstr, ..args)
  formatted := strings.to_string(builder)

  needs_escaping := false
  for char in formatted {
    if char == '&' || char == '<' || char == '>' || char == '"' {
      needs_escaping = true
      break
    }
  }

  if needs_escaping {
    // Move the formatted string outside the builder. That way the builder can
    // be reused by the escaping loop.
    unescaped, err := strings.clone(formatted, site__alloc(gen.site, .Stack))
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
  gen.site.statistics.xml_attrs += 1
  log.assert(gen.stage == .Attributes)

  site__frame(gen.site)
  escaped := xml__escape(gen, fstr, ..args)
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
  site__frame(gen.site)
  xml__raw_string(gen, xml__escape(gen, fstr, ..args))
}

// We return a boolean such that this can be used with if statements.
@(deferred_in=xml__tag_end_auto)
xml__tag :: proc(
  gen: ^Xml_Gen, name: string, single := false, auto_close := true
) -> bool {
  log.assert(name != {})
  gen.site.statistics.xml_tags += 1
  xml__ensure_content(gen)
  fmt.sbprintf(&gen.builder, "<%v", name)
  gen.stage = .Attributes
  gen.single = single
  return true
}

xml__tag_end_auto :: proc(
  gen: ^Xml_Gen, name: string, single, auto_close: bool
) {
  if !auto_close do return
  xml__tag_end(gen, name)
}

xml__tag_end :: proc(gen: ^Xml_Gen, name: string) {
  if gen.single {
    gen.stage = .Content
    fmt.sbprintf(&gen.builder, "/>")
  } else {
    xml__ensure_content(gen)
    fmt.sbprintf(&gen.builder, "</%v>", name)
  }

  gen.single = false
}

@(private="file")
xml__output :: proc(gen: ^Xml_Gen) -> string {
  allocator := site__alloc(gen.site)
  clone, err := strings.clone(strings.to_string(gen.builder), allocator)
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

    defer {
      err := os.close(file)
      if err != nil {
        site__errorf(site, directory, "Failed to close directory: %v", err)
      }
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
            push(&site.pages, page)
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
	tokens:              uint,
	codecs:              uint,
	codec_evaluations:   uint,
  xml_tags:            uint,
  xml_attrs:           uint,
  pages:               uint,
  directories_visited: uint,
  files_generated:     uint,

  system_arena:       Bytes,
  site_forever_arena: Bytes,
  site_stack_arena:   Bytes,
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
  site__frame(site)
  g := xml__make(site)

  xml__raw_string(
    g,
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  )

  if xml__tag(g, "urlset") {
    xml__attr(g, "xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")
    for iter := iter__mk(site.pages); page in iter__next(&iter) {
      page.public or_continue
      xml__tag(g, "url")

      if xml__tag(g, "loc") {
        xml__string(g, page.url)
      }

      if xml__tag(g, "last_mod") {
        at := page__last_updated(page^)
        xml__string(g, Rfc3339(at))
      }

      if mem__non_zero(page.priority) {
        if xml__tag(g, "priority") do xml__string(g, page.priority)
      }

      if mem__non_zero(page.changefreq) {
        if xml__tag(g, "changefreq") do xml__string(g, page.changefreq)
      }
    }
  }

  return xml__output(g)
}
// }}}
// {{{ RSS feed
@(private = "file")
site__feed :: proc(
  site: ^Site, base: Page, feed: Def__Feed
) -> string {
  site__frame(site)
  g := xml__make(site)

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
          if mem__is_zero(page.description) {
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

        for iter := iter__mk(page.tags); tag in iter__next(&iter) {
          xml__tag(g, "category")
          xml__string(g, tag^)
        }
      }

      if last_update != {} {
        if xml__tag(g, "lastBuildDate") {
          xml__string(g, Rfc2822(last_update))
        }
      }
    }
  }

  return xml__output(g)
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
  push(&site.files, entry)
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
        dir := site__ipath(site, Path__Absolute(dir))
        site__errorf(site, dir, "Failed to create directory: %v", err)
        continue
      }
    }

    if os.exists(string(full_path)) {
      site__errorf(site, entry.path, "File already exists")
      continue
    }

    switch inner in entry.content {
    case string:
      err = os.write_entire_file_from_string(string(full_path), inner)
      if err != nil {
        site__errorf(site, entry.path, "Failed to write file: %v", err)
        continue
      }
    case Path__Input:
      path := site__absolute(site, site.content_root, inner, .Stack)
      err = os.copy_file(string(full_path), string(path))
      if err != nil {
        site__errorf(site, entry.path, "Failed to copy file: %v", err)
        continue
      }
    }
  }
}
// }}}
// {{{ Site
site__generate :: proc(site: ^Site) {
  site__add_file(site, Path__Output("sitemap.xml"), site__sitemap(site))
  for iter := iter__mk(site.pages); page in iter__next(&iter) {
    for iter := iter__mk(page.feeds); feed in iter__next(&iter) {
      feed_content := site__feed(site, page^, feed^)
      site__add_file(site, feed.site_path, feed_content)
    }

    page_path := site__resolve(site, page.site_path, "index.html")

    {
      site__frame(site)
      g := xml__make(site)
      page__html(g, page, .Self)
      site__add_file(site, page_path, xml__output(g))
    }

    for iter := iter__mk(page.styles); style in iter__next(&iter) {
      in_path := site__resolve(site, page.source_path, style.at)
      site__add_file(site, style.site_path, in_path)
    }

    for iter := iter__mk(page.icons); icon in iter__next(&iter) {
      in_path := site__resolve(site, page.source_path, icon.at)
      site__add_file(site, icon.site_path, in_path)
    }

    for iter := iter__mk(page.assets); asset in iter__next(&iter) {
      in_path := site__resolve(site, page.source_path, asset.from)
      out_path := site__resolve(site, page.site_path, asset.to)
      site__add_file(site, out_path, in_path)
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
        allocator = site__alloc(site)
      )
    )
  }
}
// }}}
