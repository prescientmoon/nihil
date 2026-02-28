package anima

import "core:fmt"
import "core:log"
import "core:mem/virtual"

main :: proc() {
	context.logger = log.create_console_logger()

	stats: Statistics
	kit: Codec_Kit
	codec__mk_kit(&kit, &stats, Page)
	codec := codec__block_markup(&kit)
	virtual.arena_destroy(&kit.memo_arena)
	defer virtual.arena_destroy(&kit.codec_arena)

	source := #load("./example.anima", string)
	parser: Parser
	parser__make(&parser, &stats)
	ok := parser__lex(&parser, source)
	assert(ok, "Failed to lex file")

  page := page__make(virtual.arena_allocator(&parser.output_arena))

	raw_output, consumed := codec__eval(&parser, codec, &page)
	virtual.arena_destroy(&parser.codec_output_stack)
	virtual.arena_destroy(&parser.codec_state_stack)
	defer virtual.arena_destroy(&parser.internal_arena)
	defer virtual.arena_destroy(&parser.error_arena)
	defer virtual.arena_destroy(&parser.output_arena)

	tok := exparr__get(parser.tokens, parser.token)
	if parser.errors.len > 0 {
		for i in 0 ..< parser.errors.len {
			err := exparr__get(parser.errors, i)
			log.error(err.msg, err.tok)
		}
	} else if tok.kind != .Eof {
		log.error("File was not entirely consumed: ", tok)
	} else {
		log.info("Finished parsing")
		output: ^Block_Markup = cast(^Block_Markup)raw_output
		fmt.println(mps__block_markup_to_string(output^))
	}

	log.info(stats, parser.output_arena.total_used, parser.internal_arena.total_used)
}
