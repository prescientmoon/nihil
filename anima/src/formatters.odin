package anima

import "core:fmt"
import "core:mem"
import "core:io"
import "core:time"

ELLIPSIS_SYMBOL :: '…'
QUOTE_EN_LEFT   :: '“'
QUOTE_EN_RIGHT  :: '”'

// Inserted in the page when errors are encountered
ERROR_TEXT  :: "🚨 ERROR 🚨"

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
Rfc3339 :: distinct time.Time

Date__Pretty ::      distinct time.Time
Date__Compact ::     distinct time.Time
Datetime__Pretty ::  distinct time.Time
Datetime__Compact :: distinct time.Time
// }}}
// {{{ Formatter freezing
// Function pointer war crimes!
//
// Note that we use raw pointers such that we don't have to keep track of the
// types at the type level, since that would lead to ugly type definitions.
//
// A "nicer" solution would be to simply copy the arguments in some scratch
// arena, but I don't want to think about that right now...
Frozen :: struct {
	formatter: rawptr, // Function pointer taking user_data as argument(s).
  user_data: [dynamic; 4]rawptr,
}

fmt__freeze1 :: proc(
  data: ^$A,
  formatter: proc(fi: ^fmt.Info, arg: ^A),
) -> (frozen: Frozen) {
  frozen.formatter = cast(rawptr)formatter
  push(&frozen.user_data, data)
  return frozen
}

fmt__freeze2 :: proc(
  data1: ^$A, data2: ^$B,
  formatter: proc(fi: ^fmt.Info, arg1: ^A, arg2: ^B),
) -> (frozen: Frozen) {
  frozen.formatter = cast(rawptr)formatter
  push(&frozen.user_data, data1)
  push(&frozen.user_data, data2)
  return frozen
}

fmt__freeze3 :: proc(
  data1: ^$A, data2: ^$B, data3: ^$C,
  formatter: proc(fi: ^fmt.Info, arg1: ^A, arg2: ^B, arg3: ^C),
) -> (frozen: Frozen) {
  frozen.formatter = cast(rawptr)formatter
  push(&frozen.user_data, data1)
  push(&frozen.user_data, data2)
  push(&frozen.user_data, data3)
  return frozen
}

fmt__freeze4 :: proc(
  data1: ^$A, data2: ^$B, data3: ^$C, data4: ^$D,
  formatter: proc(fi: ^fmt.Info, arg1: ^A, arg2: ^B, arg3: ^C, arg4: ^D),
) -> (frozen: Frozen) {
  frozen.formatter = cast(rawptr)formatter
  push(&frozen.user_data, data1)
  push(&frozen.user_data, data2)
  push(&frozen.user_data, data3)
  push(&frozen.user_data, data4)
  return frozen
}
// }}}

// NOTE: prefer allocating inside an arena, thus making the cleanup "free".
formatters__init :: proc(allocator: mem.Allocator) {
  formatters := new(map[typeid]fmt.User_Formatter, allocator)
  formatters^ = make(map[typeid]fmt.User_Formatter, allocator)
	fmt.set_user_formatters(formatters)

	// {{{ Bytes
	fmt.register_user_formatter(Bytes, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			bytes := cast(^Bytes)arg.data
      (verb == 'v') or_return

      units := [?]string{"B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"}

      unit_index := 0
      size := f32(bytes^)

      for size >= 1024 && unit_index < len(units) - 1 {
        size /= 1024.0
        unit_index += 1
      }

      fmt.wprintf(fi.writer, "%.2f%s", size, units[unit_index])

      return true
		},
	)
	// }}}
	// {{{ Rfc2822
	fmt.register_user_formatter(
		Rfc2822,
		proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			ts := (cast(^time.Time)arg.data)^
      (verb == 'v') or_return

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
		},
	)
	// }}}
	// {{{ Rfc3339
	fmt.register_user_formatter(
		Rfc3339,
		proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			ts := (cast(^time.Time)arg.data)^
      (verb == 'v') or_return

      hour, min, sec := time.clock_from_time(ts)

      fmt.wprintf(
        fi.writer,
        "%04i-%02i-%02iT%02i:%02i:%02iZ",
        time.year(ts),
        time.month(ts),
        time.day(ts),
        hour,
        min,
        sec,
      )

      return true
		},
	)
	// }}}
	// {{{ Date__Pretty
	fmt.register_user_formatter(
		Date__Pretty,
		proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			ts := (cast(^time.Time)arg.data)^
      (verb == 'v') or_return

      fmt.wprintf(
        fi.writer,
        "%v, %02i %v %04i",
        SHORT_WEEKDAY_NAMES[time.weekday(ts)],
        time.day(ts),
        SHORT_MONTH_NAMES[time.month(ts)],
        time.year(ts),
      )

      return true
    },
	)
	// }}}
	// {{{ Date__Compact
	fmt.register_user_formatter(
		Date__Compact,
		proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			ts := (cast(^time.Time)arg.data)^
      (verb == 'v') or_return

      fmt.wprintf(
        fi.writer,
        "%04i/%02i/%02i",
        time.year(ts),
        time.month(ts),
        time.day(ts),
      )

      return true
    },
	)
	// }}}
	// {{{ Datetime__Pretty
	fmt.register_user_formatter(
		Datetime__Pretty,
		proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			ts := (cast(^time.Time)arg.data)^
      (verb == 'v') or_return

      hour, min, _ := time.clock_from_time(ts)

      fmt.wprintf(
        fi.writer,
        "%v, %02i %v %04i at %02i:%02i",
        SHORT_WEEKDAY_NAMES[time.weekday(ts)],
        time.day(ts),
        SHORT_MONTH_NAMES[time.month(ts)],
        time.year(ts),
        hour,
        min
      )

      return true
    },
	)
	// }}}
	// {{{ Datetime__Compact
	fmt.register_user_formatter(
		Datetime__Compact,
		proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			ts := (cast(^time.Time)arg.data)^
      (verb == 'v') or_return

      hour, min, _ := time.clock_from_time(ts)

      fmt.wprintf(
        fi.writer,
        "%04i/%02i/%02i %02i:%02i",
        time.year(ts),
        time.month(ts),
        time.day(ts),
        hour,
        min
      )

      return true
    },
	)
	// }}}
	// {{{ Frozen
	fmt.register_user_formatter(
		Frozen,
		proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
			frozen := (cast(^Frozen)arg.data)^
      (verb == 'v') or_return

      d := &frozen.user_data
      switch len(d) {
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
		},
	)
	// }}}
}
