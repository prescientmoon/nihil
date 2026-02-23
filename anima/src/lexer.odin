package anima

import "base:runtime"
import "core:mem/virtual"
import "core:strings"
import "core:unicode/utf8"

Source_Loc :: struct {
	index: uint,
	line:  uint,
	col:   uint,
}

// Note that the "content" property will only be non-null in the case of spacing,
// words, and apparitions.
Token :: Token_Of(Token_Kind)
Token_Of :: struct($Kind: typeid) {
	from:    Source_Loc,
	content: string,
	kind:    Kind,
}

Token_Kind :: enum {
	None = 0,
	Word,
	Bang,
	LCurly,
	RCurly,
	Apparition,
	Newline,
	Space,
	Eof,
}

// The lexer for the anima language. The "curr" propery contains the rune
// currently being focused. The lexer works in a streaming fashion, by
// incrementally moving the focus forwards.
//
// The "next_index" is required for handling multi-byte characters.
//
// If any of the underlying functions error out, a proper error string (together
// with the source location) will get saved in the "error" property, and the
// "ok" boolean of the given function will be returned as "false".
Lexer :: struct {
	escaped_strings: virtual.Arena,
	source:          string,
	pos:             Source_Loc,
	curr:            rune,
	next_index:      uint,
	error:           struct {
		pos: Source_Loc,
		msg: string,
	},
}

mk_lexer :: proc(source: string) -> (lexer: Lexer, ok: bool) {
	lexer = Lexer {
		source = source,
		pos = Source_Loc{line = 1, col = 0, index = 0},
		curr = 0,
		next_index = 0,
	}

	err := virtual.arena_init_static(&lexer.escaped_strings)
	assert(err == nil)

	advance_rune(&lexer) or_return

	return lexer, true
}

@(require_results)
@(private = "file")
advance_rune :: proc(lexer: ^Lexer) -> (ok: bool) {
	if lexer.next_index >= len(lexer.source) {
		lexer.pos.index = len(lexer.source)

		if lexer.curr == '\n' {
			lexer.pos.line += 1
			lexer.pos.col = 1
		}

		lexer.next_index = ~uint(0)
		lexer.curr = {}
		return true
	}

	lexer.pos.index = lexer.next_index
	if lexer.curr == '\n' {
		lexer.pos.col = 1
		lexer.pos.line += 1
	} else {
		lexer.pos.col += 1
	}

	r, w := rune(lexer.source[lexer.next_index]), 1
	switch {
	case r == 0:
		lexer.error = {lexer.pos, "Illegal character NUL"}
		return false
	case r >= utf8.RUNE_SELF:
		r, w = utf8.decode_rune_in_string(lexer.source[lexer.next_index:])
		if r == utf8.RUNE_ERROR && w == 1 {
			lexer.error = {lexer.pos, "Illegal UTF-8 encoding"}
			return false
		} else if r == utf8.RUNE_BOM && lexer.next_index > 0 {
			lexer.error = {lexer.pos, "Illegal byte order mark"}
			return false
		}
	}

	lexer.next_index += uint(w)
	lexer.curr = r

	return true
}

// Look at the lexer's next rune without actively focusing it.
@(private = "file")
peek_rune :: proc(lexer: Lexer) -> rune {
	copy := lexer
	ok := advance_rune(&copy)
	if ok {
		return copy.curr
	} else {
		return {}
	}
}

// Check whether the lexer's focused rune is a valid word character. This will
// also handle escaping any of the special syntax characters like { or \.
@(private = "file")
next_rune_is_text_char :: proc(lexer: Lexer) -> (width: uint) {
	nch := peek_rune(lexer)
	switch lexer.curr {
	case {}:
		return 0
	case '\\':
		switch nch {
		case '{', '}', '!', '\\', '\n', '\r', '\t', ' ':
			return 2
		case:
			return 0
		}
	case '{', '}', '!', '\n', '\r', '\t', ' ':
		return 0
	case:
		return 1
	}
}

// Consumes any character allowed inside a word (see `next_rune_is_text_char`),
// then resolves any escaped characters. The output string will only cause an
// allocation if character escapes are encountered.
@(private = "file")
consume_word_chars :: proc(lexer: ^Lexer) -> (s: string, ok: bool) {
	copy := lexer^

	no_escapes := true

	// Traverse word once in order to compute the maximum size taken by the output
	// string
	for {
		width := next_rune_is_text_char(copy)
		(width > 0) or_break
		for _ in 0 ..< width do advance_rune(&copy) or_return
		no_escapes &&= width == 1
	}

	if no_escapes {
		string := lexer.source[lexer.pos.index:copy.pos.index]
		lexer^ = copy
		return string, true
	}

	// Allocate builder for the output string. Should never grow past the given
	// size!
	builder := strings.builder_make_len_cap(
		0,
		int(copy.pos.index - lexer.pos.index),
		virtual.arena_allocator(&lexer.escaped_strings),
	)

	// Sanity check: attempting to re-allocate the buffer will cause a panic!
	builder.buf.allocator = runtime.panic_allocator()

	for {
		width := next_rune_is_text_char(lexer^)

		assert(width <= 2)
		(width > 0) or_break

		// Escaped: skip the backslash
		if width == 2 do advance_rune(lexer) or_return

		strings.write_rune(&builder, lexer.curr)
		advance_rune(lexer) or_return
	}

	return strings.to_string(builder), true
}

tokenize :: proc(lexer: ^Lexer) -> (tok: Token, ok: bool) {
	// Skip all \r characters. Such characters cannot currently be escaped. I
	// don't use windows, so I do not care in the end.
	for lexer.curr == '\r' do advance_rune(lexer) or_return

	tok.from = lexer.pos
	tok.kind = .Eof

	char := lexer.curr
	switch char {
	case {}:
		break
	case '{', '}', '!':
		@(rodata, static)
		KIND_TABLE: [128]Token_Kind = {
			'{' = .LCurly,
			'}' = .RCurly,
			'!' = .Bang,
		}

		tok.kind = KIND_TABLE[char]
		advance_rune(lexer) or_return
		tok.content = lexer.source[tok.from.index:][:1]
	case '\n':
		tok.kind = .Newline
		advance_rune(lexer) or_return
	case '\t', ' ':
		tok.kind = .Space
		curr := lexer.pos.index
		for lexer.curr == '\t' || lexer.curr == ' ' do advance_rune(lexer) or_return
		tok.content = lexer.source[curr:lexer.pos.index]
	case:
		if next_rune_is_text_char(lexer^) > 0 {
			tok.kind = .Word
			tok.content = consume_word_chars(lexer) or_return
		} else {
			assert(lexer.curr == '\\')
			tok.kind = .Apparition
			advance_rune(lexer) or_return
			tok.content = consume_word_chars(lexer) or_return
		}
	}

	return tok, true
}
