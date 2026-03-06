package anima

import "core:fmt"
import "core:mem"
import "core:io"
import "core:time"

// {{{ Date/time types & constants
@(rodata, private="file")
SHORT_WEEKDAY_NAMES := [time.Weekday]string{
  .Monday    = "Mon",
  .Tuesday   = "Tue",
  .Wednesday = "Wed",
  .Thursday  = "Thu",
  .Friday    = "Fri",
  .Saturday  = "Sat",
  .Sunday    = "Sun",
}

@(rodata, private="file")
SHORT_MONTH_NAMES := [time.Month]string{
  .January   = "Jan",
  .February  = "Feb",
  .March     = "Mar",
  .April     = "Apr",
  .May       = "May",
  .June      = "Jun",
  .July      = "Jul",
  .August    = "Aug",
  .September = "Sep",
  .October   = "Oct",
  .November  = "Nov",
  .December  = "Dec",
}

Rfc2822 :: distinct time.Time // https://www.rfc-editor.org/rfc/rfc2822.html
// }}}

formatters__init :: proc(allocator: mem.Allocator) {
  formatters := new(map[typeid]fmt.User_Formatter, allocator)
  formatters^ = make(map[typeid]fmt.User_Formatter, allocator)
	fmt.set_user_formatters(formatters)

	// {{{ Bytes
	fmt.register_user_formatter(
		Bytes,
		proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			bytes := cast(^Bytes)arg.data

			switch verb {
			case 'v':
				units := [?]string{"B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"}

				unit_index := 0
				size := f32(bytes^)

				for size >= 1024 && unit_index < len(units) - 1 {
					size /= 1024.0
					unit_index += 1
				}

				fmt.wprintf(fi.writer, "%.2f%s", size, units[unit_index])

				return true
			case:
				return false
			}
		},
	)
	// }}}
	// {{{ RFC2822
	fmt.register_user_formatter(
		Rfc2822,
		proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			ts := (cast(^time.Time)arg.data)^

			switch verb {
			case 'v':
        hour, min, sec := time.clock_from_time(ts)

        fmt.wprintf(
          fi.writer,
          "%v, %02i %v %04i %02i:%02i:%02i +0000",
          SHORT_WEEKDAY_NAMES[time.weekday(ts)],
          time.day(ts),
          SHORT_MONTH_NAMES[time.month(ts)],
          time.year(ts),
          hour,
          min,
          sec,
        )

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
