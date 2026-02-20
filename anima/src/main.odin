package anima

import "core:fmt"
import "core:log"

main :: proc() {
	context.logger = log.create_console_logger()
	source :: #load("./example2.anima", string)

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
			fmt.println(mps_apf_to_string(tree.roots))

			log.info("Evaluator results:")
			eval: Evaluator
			eval.allocator = parser.allocator
			eval.internal_allocator = parser.internal_allocator
			eval.errors = new(Exparr(Parsing_Error), allocator = parser.internal_allocator)
			eval.errors.allocator = eval.allocator
			eval.scope = new(Scope, allocator = parser.internal_allocator)
			eval.caller = eval.scope
			eval.scope.members.allocator = eval.allocator

			spec: Apparition_Spec = Rigid_Apparition_Spec{}

			args := mk_apparition_parsing_args(eval, spec, {})
			apt_parse(eval, spec, tree.roots, args)

			if eval.errors.len > 0 {
				log.infof("Evaluation failed with %v error(s):", eval.errors.len)
				for i in 0 ..< eval.errors.len {
					fmt.println(exparr_get(eval.errors^, i))
				}
			} else {
				log.info("Evaluation was successfull!")
				for i in 0 ..< args[0].len {
					arg := exparr_get(args[0], i)
					// fmt.println(arg)
					fmt.println(mps_dapf_to_string(arg.value))
				}
				// fmt.println(args[0])
				// fmt.println(apt_eval_string(eval, args[0]))
			}
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
