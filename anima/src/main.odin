package anima

import "core:fmt"

main :: proc() {
	init_formatters()
	defer deinit_formatters()

	exparr: Exparr(uint, 3)
	exparr.allocator = context.temp_allocator

	for i in 0 ..< 1024 {
		exparr_push(&exparr, uint(i))
	}

	exparr_pop(&exparr)
	exparr_pop(&exparr)
	exparr_pop(&exparr)

	lexer, lexer_ok := mk_lexer(#load("./example.anima"))
	assert(lexer_ok)
	for tok in tokenize(&lexer) {
		fmt.println(tok)
		if tok.kind == .Eof do break
	}

	parser, ok := mk_parser(#load("./example.anima"))
	assert(ok)

	blocks, blocks_ok := parse_blocks(&parser)
	if parser.error == {} {
		tok, found, ok := expect_token(&parser, .Eof)
		if !found || !ok {
			parser.error = {tok, "I'm confused by this token"}
		}
	}
	fmt.println(blocks)
	if parser.lexer.error != {} {
		fmt.println(parser.lexer.error)
	} else if parser.error != {} {
		fmt.println(parser.error)
	}
}
