package anima

import "core:fmt"
import "core:log"
import "core:mem/virtual"

main :: proc() {
	context.logger = log.create_console_logger()

	kit: Codec_Kit
	codec__mk_kit(&kit)
	codec := codec__inline_markup(&kit)
	virtual.arena_destroy(&kit.memo_arena)
	defer virtual.arena_destroy(&kit.codec_arena)

	source := #load("./example.anima", string)
	parser: Parser
	ok := mk_parser(source, &parser)
	assert(ok, "Failed to create parser")

	raw_output, _ := codec_eval(&parser, codec)
	virtual.arena_destroy(&parser.codec_output_stack)
	virtual.arena_destroy(&parser.codec_state_stack)
	virtual.arena_destroy(&parser.internal_arena)
	defer virtual.arena_destroy(&parser.error_arena)
	defer virtual.arena_destroy(&parser.output_arena)
	defer virtual.arena_destroy(&parser.lexer.escaped_strings)

	if parser.errors.len > 0 {
		for i in 0 ..< parser.errors.len {
			err := exparr__get(parser.errors, i)
			log.error(err.msg, err.tok)
		}
	} else {
		output: ^Inline_Markup = cast(^Inline_Markup)raw_output
		log.debug("Finished")
		fmt.println(mps__inline_markup_to_string(output^))
	}
}
