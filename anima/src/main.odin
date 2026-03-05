package anima

import "core:fmt"
import "core:log"
import "core:mem"
import "core:mem/virtual"

main :: proc() {
  context.allocator = mem.panic_allocator()
	context.logger = log.create_console_logger(allocator=context.temp_allocator)
  defer log.destroy_console_logger(context.logger, allocator=context.temp_allocator)

	stats: Statistics

	kit: Codec_Kit
	codec__kit__make(&kit, &stats, Page)
  defer codec__kit__destroy(&kit)

	parser: Parser
	parser__make(&parser, &stats)
  defer parser__destroy(&parser)

  file := File {
    source = #load("./example.anima", string),
    name   = "internal"
  }

	ok := parser__lex(&parser, &file)
	assert(ok, "Failed to lex file")

	codec := codec__page(&kit)
	raw_output, _ := codec__eval(&parser, codec)

	tok := exparr__get(parser.tokens, parser.token)
	if parser.errors.len > 0 {
		for i in 0 ..< parser.errors.len {
			err := exparr__get(parser.errors, i)^
      fmt.println(pretty_error(err))
		}
	} else if tok.kind != .Eof {
		log.error("File was not entirely consumed: ", tok)
	} else {
		log.info("Finished parsing")
		output := cast(^Page)raw_output
		fmt.println(mps__page_to_string(output^))
	}

	log.info(
    stats,
    parser.output_arena.total_used,
    parser.internal_arena.total_used,
  )
}
