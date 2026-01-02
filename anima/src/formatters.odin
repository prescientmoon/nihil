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
		markup := cast(^Inline_Markup)arg.data

		switch verb {
		case 'v':
			#partial switch markup.kind {
			case .None:
				fmt.wprintf(fi.writer, "<none>")
			case .Space:
				fmt.wprintf(fi.writer, " ")
			case .Text:
				fmt.wprintf(fi.writer, "<'%v'>", markup.raw)
			case .Strong:
				fmt.wprintf(fi.writer, "<*%v*>", markup.inner)
			case .Emph:
				fmt.wprintf(fi.writer, "<*%v*>", markup.inner)
			case .Strikethrough:
				fmt.wprintf(fi.writer, "<_%v_>", markup.inner)
			case .Mono:
				fmt.wprintf(fi.writer, "<`%v`>", markup.inner)
			case .Quote:
				fmt.wprintf(fi.writer, "<\"%v\">", markup.inner)
			case .LaTeX:
				fmt.wprintf(fi.writer, "<$%v$>", markup.raw)
			case .Link:
				fmt.wprintf(fi.writer, "<link: %v>", markup.link)
			case .Fn:
				fmt.wprintf(fi.writer, "<fn: %v>", markup.raw)
			case .Icon:
				fmt.wprintf(fi.writer, "<icon: %v>", markup.raw)
			case .Date:
				fmt.wprintf(fi.writer, "<date: %v>", markup.time)
			case .Datetime:
				fmt.wprintf(fi.writer, "<datetime: %v>", markup.time)
			case .Many:
				fmt.wprintf(fi.writer, "<many: ")

				for i in 0 ..< markup.many.len {
					inner := exparr_get(&markup.many, i)
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
	// {{{ block markup
	fmt.register_user_formatter(^Block_Markup, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		inner := cast(^^Block_Markup)arg.data
		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "%v", inner^^)
			return true
		case:
			return false
		}
	})

	fmt.register_user_formatter(Block_Markup, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		markup := cast(^Block_Markup)arg.data

		switch verb {
		case 'v':
			#partial switch markup.kind {
			case .Thematic_Break:
				fmt.wprintf(fi.writer, "<--->")
			case .Toc:
				fmt.wprintf(fi.writer, "<toc>")
			case .Embed_Description:
				fmt.wprintf(fi.writer, "<embed-description>")
			case .Paragraph:
				fmt.wprintf(fi.writer, "<para: %v>", markup.paragraph)
			case .Blockquote:
				fmt.wprintf(fi.writer, "<blockquote: %v>", markup.blockquote)
			case .Heading:
				fmt.wprintf(
					fi.writer,
					"<h%v: %v #%v>",
					markup.heading.level,
					markup.heading.contents,
					markup.heading.id,
				)
			case .Image:
				fmt.wprintf(fi.writer, "<img: %v>", markup.image)
			case .Aside:
				fmt.wprintf(fi.writer, "<aside: %v>", markup.aside)
			case .Many:
				fmt.wprintf(fi.writer, "<many: ")

				for i in 0 ..< markup.many.len {
					inner := exparr_get(&markup.many, i)
					fmt.wprintf(fi.writer, "%v", inner)
				}

				fmt.wprintf(fi.writer, ">")
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
