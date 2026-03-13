package anima

import "core:fmt"
import "core:mem"
import "core:io"
import "core:time"
import "core:container/small_array"

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
// {{{ Frozen formatters
Frozen :: struct {
	formatter: rawptr,
  user_data: small_array.Small_Array(4, rawptr),
}

fmt__freeze1 :: proc(
  data: ^$A,
  formatter: proc(fi: ^fmt.Info, arg: ^A),
) -> (frozen: Frozen) {
  frozen.formatter = cast(rawptr)formatter
  small_array.push_back(&frozen.user_data, data)
  return frozen
}

fmt__freeze2 :: proc(
  data1: ^$A, data2: ^$B,
  formatter: proc(fi: ^fmt.Info, arg1: ^A, arg2: ^B),
) -> (frozen: Frozen) {
  frozen.formatter = cast(rawptr)formatter
  small_array.push_back(&frozen.user_data, data1)
  small_array.push_back(&frozen.user_data, data2)
  return frozen
}

fmt__freeze3 :: proc(
  data1: ^$A, data2: ^$B, data3: ^$C,
  formatter: proc(fi: ^fmt.Info, arg1: ^A, arg2: ^B, arg3: ^C),
) -> (frozen: Frozen) {
  frozen.formatter = cast(rawptr)formatter
  small_array.push_back(&frozen.user_data, data1)
  small_array.push_back(&frozen.user_data, data2)
  small_array.push_back(&frozen.user_data, data3)
  return frozen
}

fmt__freeze4 :: proc(
  data1: ^$A, data2: ^$B, data3: ^$C, data4: ^$D,
  formatter: proc(fi: ^fmt.Info, arg1: ^A, arg2: ^B, arg3: ^C, arg4: ^D),
) -> (frozen: Frozen) {
  frozen.formatter = cast(rawptr)formatter
  small_array.push_back(&frozen.user_data, data1)
  small_array.push_back(&frozen.user_data, data2)
  small_array.push_back(&frozen.user_data, data3)
  return frozen
}
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
	// {{{ Frozen
	fmt.register_user_formatter(
		Frozen,
		proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			frozen := (cast(^Frozen)arg.data)^

			switch verb {
			case 'v':
        d := frozen.user_data.data
        switch frozen.user_data.len {
        case 0:
          f := cast(proc(fi: ^fmt.Info))frozen.formatter
          f(fi)
        case 1:
          f := cast(proc(fi: ^fmt.Info, a1: rawptr))frozen.formatter
          f(fi, d[0])
        case 2:
          f := cast(proc(fi: ^fmt.Info, a1, a2: rawptr))frozen.formatter
          f(fi, d[0], d[1])
        case 3:
          f := cast(proc(fi: ^fmt.Info, a1, a2, a3: rawptr))frozen.formatter
          f(fi, d[0], d[1], d[2])
        case 4:
          f := cast(proc(fi: ^fmt.Info, a1, a2, a3, a4: rawptr))frozen.formatter
          f(fi, d[0], d[1], d[2], d[3])
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
