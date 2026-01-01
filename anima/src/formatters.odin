package anima

import "core:fmt"
import "core:io"

init_formatters :: proc() {
	fmt.set_user_formatters(new(map[typeid]fmt.User_Formatter))

	// {{{ inline markup
	fmt.register_user_formatter(^Inline_Markup, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		inner := cast(^^Inline_Markup)arg.data
		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "%v", inner^^)
			return true
		case:
			return false
		}
	})

	fmt.register_user_formatter(Inline_Markup, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		inline_markup := cast(^Inline_Markup)arg.data

		switch verb {
		case 'v':
			#partial switch inline_markup.kind {
			case .None:
				fmt.wprintf(fi.writer, "<none>")
			case .Space:
				fmt.wprintf(fi.writer, " ")
			case .Text:
				fmt.wprintf(fi.writer, "<'%v'>", inline_markup.raw)
			case .Strong:
				fmt.wprintf(fi.writer, "<*%v*>", inline_markup.inner)
			case .Emph:
				fmt.wprintf(fi.writer, "<*%v*>", inline_markup.inner)
			case .Strikethrough:
				fmt.wprintf(fi.writer, "<_%v_>", inline_markup.inner)
			case .Mono:
				fmt.wprintf(fi.writer, "<`%v`>", inline_markup.inner)
			case .Quote:
				fmt.wprintf(fi.writer, "<\"%v\">", inline_markup.inner)
			case .LaTeX:
				fmt.wprintf(fi.writer, "<$%v$>", inline_markup.raw)
			case .Link:
				fmt.wprintf(fi.writer, "<link: %v>", inline_markup.link)
			case .Fn:
				fmt.wprintf(fi.writer, "<fn: %v>", inline_markup.raw)
			case .Icon:
				fmt.wprintf(fi.writer, "<icon: %v>", inline_markup.raw)
			case .Date:
				fmt.wprintf(fi.writer, "<date: %v>", inline_markup.time)
			case .Datetime:
				fmt.wprintf(fi.writer, "<datetime: %v>", inline_markup.time)
			case .Many:
				fmt.wprintf(fi.writer, "<many: ")

				for i in 0 ..< inline_markup.many.len {
					inner := exparr_get(&inline_markup.many, i)
					fmt.wprintf(fi.writer, "%v", inner)
				}

				fmt.wprintf(fi.writer, ">")
			case .Ellipsis:
				fmt.wprintf(fi.writer, "<...>")
			case:
				fmt.wprintf(fi.writer, "<unknown>")
			}

			return true
		case:
			return false
		}
	})
	// }}}
}

deinit_formatters :: proc() {
	delete(fmt._user_formatters^)
	free(fmt._user_formatters)
}
