package anima

import "core:fmt"
import "core:mem"
import "core:io"


formatters__init :: proc(allocator: mem.Allocator) {
  formatters := new(map[typeid]fmt.User_Formatter, allocator)
  formatters^ = make(map[typeid]fmt.User_Formatter, allocator)
	fmt.set_user_formatters(formatters)

	// {{{ Contiguous_Text
	fmt.register_user_formatter(
		Contiguous_Text,
		proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			ctext := cast(^Contiguous_Text)arg.data

			switch verb {
			case 'v':
        for i in 0..<ctext.len {
          fmt.wprint(fi.writer, exparr__get(ctext^, i)^)
        }

				return true
			case:
				return false
			}
		},
	)
	// }}}
}

formatters__deinit :: proc(allocator: mem.Allocator) {
	delete(fmt._user_formatters^)
	free(fmt._user_formatters, allocator)
}
