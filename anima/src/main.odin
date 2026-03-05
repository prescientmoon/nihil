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

  site: Site
  content_root := "/home/moon/projects/personal/nihil/anima"
  site__make(&site, "https://moonythm.dev", content_root)
  site__collect(&site)

  for i in 0..<site.errors.len {
    err := exparr__get(site.errors, i)^
    fmt.eprintln(pretty_error(err))
  }

	log.info(site.statistics)
}
