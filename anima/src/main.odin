package anima

import "core:fmt"
import "core:log"
import "core:mem"
import "core:mem/virtual"

// CONSTANTS
GENERATOR   :: "anima"
USERNAME    :: "prescientmoon"
EMAIL       :: "hi@moonythm.dev"
THEME_COLOR :: "#faebff"
SITE_NAME   :: "Moonythm"
FEDI_USER   :: "@prescientmoon@moonythm.dev"

main :: proc() {
  context.allocator = mem.panic_allocator()
  defer free_all(context.temp_allocator)

  system_arena: virtual.Arena
  system_allocator := virtual.arena_allocator(&system_arena)
	err := virtual.arena_init_static(&system_arena)
	assert(err == nil)
  defer virtual.arena_destroy(&system_arena)

	context.logger = log.create_console_logger(allocator=system_allocator)
  formatters__init(system_allocator)

  site: Site
  content_root := "/home/moon/projects/personal/nihil/anima/src/example"
  out_root := "/home/moon/projects/personal/nihil/anima/dist"
  site__make(&site, "http://localhost:8080", content_root, out_root)
  site.statistics.system_arena = Bytes(system_arena.total_used)

  site__collect(&site)
  site__check_errors(&site)
  site__check(&site)
  site__check_errors(&site)
  site__generate(&site)
  site__check_errors(&site)
  site__commit(&site)
  site__check_errors(&site)
  site__destroy(&site)

	fmt.printfln("%#v", site.statistics)
}
