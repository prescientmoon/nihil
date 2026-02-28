package anima

import "core:fmt"
import "core:log"
import "core:mem/virtual"

main :: proc() {
	context.logger = log.create_console_logger()

	stats: Statistics

	kit: Codec_Kit
	codec__kit__make(&kit, &stats, Page)
  defer codec__kit__destroy(&kit)

	parser: Parser
	parser__make(&parser, &stats)
  defer parser__destroy(&parser)

	ok := parser__lex(&parser, #load("./example.anima", string))
	assert(ok, "Failed to lex file")

	codec := codec__block_markup(&kit)
  page := page__make(virtual.arena_allocator(&parser.output_arena))
	raw_output, _ := codec__eval(&parser, codec, &page)

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

	log.info(
    stats,
    parser.output_arena.total_used,
    parser.internal_arena.total_used,
  )
}
