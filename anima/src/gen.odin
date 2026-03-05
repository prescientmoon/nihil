package anima

import "core:fmt"
import "core:log"
import "core:time"
import "core:strings"
import "core:mem/virtual"

Site :: struct {
  base_url: string,
  pages: Exparr(Page),
  xml:   Xml_Gen,
}

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
  log.assert(mem__iz(gen^))
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
  epxarr__clear(&gen.tag_stack)
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

      if mem__nz(page.priority) {
        if xml__tag_begin(g, "priority") do xml__ctext(g, page.priority)
      }

      if mem__nz(page.changefreq) {
        if xml__tag_begin(g, "changefreq") do xml__ctext(g, page.changefreq)
      }
    }
  }

  return xml__save(g)
}
// }}}
