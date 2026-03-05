package anima

import "core:fmt"
import "core:log"
import "core:mem"
import "core:mem/virtual"

main :: proc() {
  context.allocator = mem.panic_allocator()

  system_arena: virtual.Arena
  system_allocator := virtual.arena_allocator(&system_arena)
	err := virtual.arena_init_static(&system_arena)
	assert(err == nil)

	context.logger = log.create_console_logger(allocator=system_allocator)
  defer log.destroy_console_logger(context.logger, allocator=system_allocator)

  formatters__init(system_allocator)
  defer formatters__deinit(system_allocator)

	stats: Statistics

	kit: Codec_Kit
	codec__kit__make(&kit, &stats, Page)
  defer codec__kit__destroy(&kit)

	parser: Parser
	parser__make(&parser, &stats)
  defer parser__destroy(&parser)

  file := File {
    source = #load("./example.anima", string),
    name   = "internal"
  }

	ok := parser__lex(&parser, &file)
	assert(ok, "Failed to lex file")

	codec := codec__page(&kit)
	raw_output, _ := codec__eval(&parser, codec)

	if parser.errors.len > 0 {
		for i in 0 ..< parser.errors.len {
			err := exparr__get(parser.errors, i)^
      fmt.println(pretty_error(err))
		}
	} else {
		log.info("Finished parsing")
		page := cast(^Page)raw_output
		fmt.println(mps__page_to_string(page^))

    site: Site
    site.pages.allocator = context.temp_allocator
    exparr__push(&site.pages, page^)
    xml__make(&site.xml, &stats)
    site.base_url = "https://moonythm.dev"
    fmt.println(site__sitemap(&site))
	}

	log.info(
    stats,
    parser.output_arena.total_used,
    parser.internal_arena.total_used,
  )
}
