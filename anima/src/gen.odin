package anima

import "core:os"
import "core:fmt"
import "core:log"
import "core:time"
import "core:strings"
import "core:mem/virtual"

// {{{ Site
Site :: struct {
  // The base url the site will be accessible at. The string should preferably 
  // not end in a trailing slash.
  base_url: string,

  // The root of the directory containing the source files to generate the
  // website out of.
  content_root: Path__Absolute,

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
  xml:        Xml_Gen,
  codec_kit:  Codec_Kit,
  page_codec: Typed_Codec(Page),
  parser:     Parser,
}

site__make :: proc(site: ^Site, base_url: string, content_root: string) {
  log.assert(mem__iz(site^))

  site.base_url     = base_url
  site.content_root = Path__Absolute(content_root)

	err := virtual.arena_init_static(&site.forever_arena)
	log.assert(err == nil)
  site.pages.allocator = virtual.arena_allocator(&site.forever_arena)
  site.errors.allocator = virtual.arena_allocator(&site.forever_arena)

	codec__kit__make(&site.codec_kit, &site.statistics)
	parser__make(&site.parser, &site.statistics)
  site.page_codec = codec__page(&site.codec_kit)
  xml__make(&site.xml, &site.statistics)
}

site__path :: proc(site: ^Site, str: string) -> Path__Absolute {
  allocator := virtual.arena_allocator(&site.forever_arena)
  clone, err := strings.clone(str, allocator)
  log.assert(err == nil)
  return Path__Absolute(clone)
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
// }}}
// {{{ Path
Path__Absolute :: distinct string
Path__Relative :: distinct string
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
  exparr__clear(&gen.tag_stack)
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
@(deferred_in=xml__tag_auto_end)
xml__tag_begin :: proc(
  gen: ^Xml_Gen, name: string, single: bool = false
) -> bool {
  gen.statistics.xml_tags += 1
  xml__ensure_content(gen)
  exparr__push(&gen.tag_stack, name)
  fmt.sbprintf(&gen.builder, "<%v", name)
  gen.stage = .Attributes
  return true
}

xml__tag_auto_end :: proc(gen: ^Xml_Gen, name: string, single: bool) {
  xml__tag_end(gen)
}

xml__tag_end :: proc(gen: ^Xml_Gen) {
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
// {{{ Sitemap
site__sitemap :: proc(site: ^Site) -> string {
  g := &site.xml
  xml__clear(g)
  xml__raw_string(
    g,
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  )

  if xml__tag_begin(g, "urlset") {
    xml__attr(g, "xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")
    for i in 0..<site.pages.len {
      page := exparr__get(site.pages, i)
      page.public or_continue
      xml__tag_begin(g, "url")

      if xml__tag_begin(g, "loc") do xml__stringf(g, "%v", site.base_url)

      if xml__tag_begin(g, "last_mod") {
        allocator := virtual.arena_allocator(&g.internal_arena)
        at := page__last_updated(page^)
        str, ok := time.time_to_rfc3339(at, allocator=allocator)
        log.assert(ok)
        xml__stringf(g, "%v", str)
      }

      if mem__non_zero(page.priority) {
        if xml__tag_begin(g, "priority") do xml__ctext(g, page.priority)
      }

      if mem__non_zero(page.changefreq) {
        if xml__tag_begin(g, "changefreq") do xml__ctext(g, page.changefreq)
      }
    }
  }

  return xml__save(g)
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
            page = cast(^Page)parser__eval(&site.parser, site.page_codec)
          }

          if site.parser.errors.len > 0 {
            exparr__push_exparr(&site.errors, site.parser.errors)
          } else {
            exparr__push(&site.pages, page^)
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
