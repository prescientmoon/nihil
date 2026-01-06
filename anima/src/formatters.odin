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
			case .None:
				fmt.wprintf(fi.writer, "<none>")
			case .Thematic_Break:
				fmt.wprintf(fi.writer, "<--->")
			case .Table_Of_Contents:
				fmt.wprintf(fi.writer, "<toc>")
			case .Embed_Description:
				fmt.wprintf(fi.writer, "<embed-description>")
			case .Paragraph:
				fmt.wprintf(fi.writer, "<para: %v>", markup.paragraph)
			case .Blockquote:
				fmt.wprintf(fi.writer, "<blockquote: %v>", markup.blockquote)
			case .Heading:
				fmt.wprintf(fi.writer, "<h%v: %v #%v>", markup.heading.level, markup.heading.contents, markup.heading.id)
			case .Image:
				fmt.wprintf(fi.writer, "<img: %v>", markup.image)
			case .Figure:
				fmt.wprintf(fi.writer, "<figure: %v>", markup.figure)
			case .Aside:
				fmt.wprintf(fi.writer, "<aside: %v>", markup.aside)
			case .Linkdef:
				fmt.wprintf(fi.writer, "<linkdef: %v>", markup.linkdef^)
			case .Fndef:
				fmt.wprintf(fi.writer, "<fndef: %v>", markup.fndef^)
			case .Codeblock:
				fmt.wprintf(fi.writer, "<codeblock: %v>", markup.codeblock)
			case .Page_Index:
				fmt.wprintf(fi.writer, "<page-index: %v>", markup.page_index)
			case .Table:
				fmt.wprintf(fi.writer, "<table: ")

				if markup.table.caption.kind != .None {
					fmt.wprintf(fi.writer, "<caption: %v>", markup.table.caption)
				}

				if markup.table.head.cells.len > 0 {
					fmt.wprintf(fi.writer, "<head: %v>", markup.table.head)
				}

				for i in 0 ..< markup.table.rows.len {
					inner := exparr_get(&markup.table.rows, i)
					fmt.wprintf(fi.writer, "%v", inner)
				}

				fmt.wprintf(fi.writer, ">")
			case .IList:
				if markup.ilist.ordered {
					fmt.wprintf(fi.writer, "<oilist: ")
				} else {
					fmt.wprintf(fi.writer, "<uilist: ")
				}

				for i in 0 ..< markup.ilist.items.len {
					item := exparr_get(&markup.ilist.items, i)
					fmt.wprintf(fi.writer, "%v", item)
					if i != markup.ilist.items.len - 1 do fmt.wprintf(fi.writer, ",")
				}

				fmt.wprintf(fi.writer, ">")
			case .BList:
				if markup.blist.ordered {
					fmt.wprintf(fi.writer, "<oblist: ")
				} else {
					fmt.wprintf(fi.writer, "<ublist: ")
				}

				for i in 0 ..< markup.blist.items.len {
					item := exparr_get(&markup.blist.items, i)
					fmt.wprintf(fi.writer, "%v", item)
					if i != markup.blist.items.len - 1 do fmt.wprintf(fi.writer, ",")
				}

				fmt.wprintf(fi.writer, ">")
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
	// {{{ Other markup
	fmt.register_user_formatter(^Table_Row, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		inner := cast(^^Table_Row)arg.data
		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "%v", inner^^)
			return true
		case:
			return false
		}
	})

	fmt.register_user_formatter(Table_Row, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		row := cast(^Table_Row)arg.data

		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "<row: ")

			for i in 0 ..< row.cells.len {
				inner := exparr_get(&row.cells, i)
				fmt.wprintf(fi.writer, "%v", inner)
			}

			fmt.wprintf(fi.writer, ">")

			return true
		case:
			return false
		}
	})

	fmt.register_user_formatter(^Table_Cell, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		inner := cast(^^Table_Cell)arg.data
		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "%v", inner^^)
			return true
		case:
			return false
		}
	})

	fmt.register_user_formatter(Table_Cell, proc(fi: ^fmt.Info, arg: any, verb: rune) -> bool {
		cell := cast(^Table_Cell)arg.data
		switch verb {
		case 'v':
			fmt.wprintf(fi.writer, "<cell: %v", cell.content)
			if cell.bg != "" {
				fmt.wprintf(fi.writer, " bg=%v", cell.bg)
			}
			fmt.wprintf(fi.writer, ">")
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
