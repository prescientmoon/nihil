package anima

import "core:fmt"
import "core:log"

main :: proc() {
	context.logger = log.create_console_logger()
	source :: #load("./example.anima", string)

	log.info("Lexer results:")
	mps := mps_init()
	mps_tokens(&mps, source)
	fmt.println(mps_to_string(mps))

	{
		log.info("Parser results:")
		parser := mk_parser(source) or_else panic("Failed parser init :(")

		tree: Apt_Storage
		tree.nodes.allocator = parser.allocator
		builder := tb_make(&tree)

		parse_apt(&parser, &builder)
		parser_end(&parser)

		if parser.lexer.error != {} {
			fmt.println(parser.lexer.error)
		} else if parser.error != {} {
			fmt.println(parser.error)
		} else {
			tree_without_comments: Apt_Storage
			tree_without_comments.nodes.allocator = parser.allocator
			builder := tb_make(&tree_without_comments)

			apf_remove_comments(&builder, tree.roots)

			mps := mps_init()
			mps_apparition_forest(&mps, tree_without_comments.roots)
			fmt.println(mps_to_string(mps))
		}
	}

	// {
	// 	parser := mk_parser(#load("./example.anima")) or_else panic("Failed parser init :(")
	//
	// 	blocks, blocks_ok := parse_blocks(&parser)
	// 	parser_end(&parser)
	//
	// 	mps := mps_init()
	// 	mps_block_markup(&mps, blocks)
	// 	fmt.println(mps_to_string(mps))
	//
	// 	if parser.lexer.error != {} {
	// 		fmt.println(parser.lexer.error)
	// 	} else if parser.error != {} {
	// 		fmt.println(parser.error)
	// 	}
	// }
}
