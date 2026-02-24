package anima

// {{{ Contiguous text
// A sequence of text where all the whitespace in the source is discarded
Contiguous_Text :: distinct Exparr(string)
codec__contiguous_text :: proc(kit: ^Codec_Kit) -> Typed_Codec(Contiguous_Text) {
	return codec__memo(
		kit,
		Contiguous_Text,
		"contiguous_text",
		proc(kit: ^Codec_Kit) -> Typed_Codec(Contiguous_Text) {
			return codec__transmute(
				kit,
				Contiguous_Text,
				codec__spaced_exparr(kit, codec__string(kit)),
			)
		},
	)
}
// }}}

// {{{ Inline
Inline_Markup__Space :: distinct Unit
Inline_Markup__Ellipsis :: distinct Unit
Inline_Markup__Text :: distinct string
Inline_Markup__Emph :: distinct Inline_Markup
Inline_Markup__Strong :: distinct Inline_Markup
Inline_Markup__Strikethrough :: distinct Inline_Markup
Inline_Markup__Mono :: distinct Inline_Markup
Inline_Markup__Quote :: distinct Inline_Markup

// Using distinct runs into circular types issue (for no reason)
Inline_Markup :: struct {
	elements: Exparr(Inline_Markup__Atom),
}

// TODO: datetime, date, time, link, fn, icon, LaTeX
Inline_Markup__Atom :: union {
	Inline_Markup__Space,
	Inline_Markup__Ellipsis,
	Inline_Markup__Text,
	Inline_Markup__Emph,
	Inline_Markup__Strong,
	Inline_Markup__Strikethrough,
	Inline_Markup__Mono,
	Inline_Markup__Quote,
}
// }}}
// {{{ Codecs
@(private = "file")
codec__inline_markup__atom :: proc(k: ^Codec_Kit) -> Typed_Codec(Inline_Markup__Atom) {
	imarkup := codec__inline_markup(k)
	space := codec__space(k, Inline_Markup__Space)
	text := codec__transmute(k, Inline_Markup__Text, codec__string(k))
	ellipsis := codec__constant(k, "...", Inline_Markup__Ellipsis{})
	emph := codec__trans_at(k, "_", Inline_Markup__Emph, imarkup)
	strong := codec__trans_at(k, "*", Inline_Markup__Strong, imarkup)
	strike := codec__trans_at(k, "~", Inline_Markup__Strikethrough, imarkup)
	mono := codec__trans_at(k, "`", Inline_Markup__Mono, imarkup)
	quote := codec__trans_at(k, "\"", Inline_Markup__Quote, imarkup)

	return codec__sum(
		k,
		Inline_Markup__Atom,
		codec__variant(k, Inline_Markup__Atom, space),
		codec__variant(k, Inline_Markup__Atom, text),
		codec__variant(k, Inline_Markup__Atom, ellipsis),
		codec__variant(k, Inline_Markup__Atom, emph),
		codec__variant(k, Inline_Markup__Atom, strong),
		codec__variant(k, Inline_Markup__Atom, strike),
		codec__variant(k, Inline_Markup__Atom, mono),
		codec__variant(k, Inline_Markup__Atom, quote),
	)
}

codec__inline_markup :: proc(kit: ^Codec_Kit) -> Typed_Codec(Inline_Markup) {
	return codec__memo(
		kit,
		Inline_Markup,
		"inline_markup",
		proc(kit: ^Codec_Kit) -> Typed_Codec(Inline_Markup) {
			return codec__transmute(
				kit,
				Inline_Markup,
				codec__exparr(kit, codec__inline_markup__atom(kit)),
			)
		},
	)
}
// }}}

// {{{ Block
Block_Markup__Paragraph :: distinct Inline_Markup

Block_Markup__Image :: struct {
	alt:    Inline_Markup,
	source: Contiguous_Text,
}

Block_Markup__Figure :: struct {
	caption: Inline_Markup,
	content: Block_Markup,
}

Block_Markup__List :: struct {
	ordered:  bool,
	block:    bool,
	elements: union #no_nil {
		Exparr(Inline_Markup),
		Exparr(Block_Markup),
	},
}

Block_Markup__Blockquote :: distinct Block_Markup
Block_Markup__Description :: distinct Unit
Block_Markup__Table_Of_Contents :: distinct Unit
Block_Markup__Thematic_Break :: distinct Unit

// TODO: heading, code, aside, index, link/fn defs
Block_Markup__Atom :: union {
	Block_Markup__Paragraph,
	Block_Markup__Image,
	Block_Markup__Figure,
	Block_Markup__List, // todo
	Block_Markup__Blockquote,
	Block_Markup__Description,
	Block_Markup__Table_Of_Contents,
	Block_Markup__Thematic_Break,
	Table,
}

Block_Markup :: struct {
	elements: Exparr(Block_Markup__Atom),
}
// }}}
// {{{ Codecs
@(private = "file")
codec__block_markup__image :: proc(k: ^Codec_Kit) -> Typed_Codec(Block_Markup__Image) {
	source := codec__at(k, "source", codec__once(k, codec__contiguous_text(k)))
	source_ref := codec__field(k, "source", Block_Markup__Image, source)
	alt := codec__inline_markup(k)
	alt_ref := codec__field(k, "alt", Block_Markup__Image, alt)
	return codec__loop(k, codec__sum(k, Block_Markup__Image, source_ref, alt_ref))
}

@(private = "file")
codec__block_markup__figure :: proc(k: ^Codec_Kit) -> Typed_Codec(Block_Markup__Figure) {
	caption := codec__at(k, "caption", codec__once(k, codec__inline_markup(k)))
	caption_ref := codec__field(k, "caption", Block_Markup__Figure, caption)
	content := codec__field(k, "content", Block_Markup__Figure, codec__block_markup(k))
	return codec__loop(k, codec__sum(k, Block_Markup__Figure, caption_ref, content))
}

@(private = "file")
codec__block_markup__atom :: proc(k: ^Codec_Kit) -> Typed_Codec(Block_Markup__Atom) {
	imarkup := codec__inline_markup(k)
	bmarkup := codec__block_markup(k)
	description := codec__constant(k, "embed-description", Block_Markup__Description{})
	thematic_break := codec__constant(k, "---", Block_Markup__Thematic_Break{})
	table_of_contents := codec__constant(k, "toc", Block_Markup__Table_Of_Contents{})
	blockquote := codec__trans_at(k, ">", Block_Markup__Blockquote, bmarkup)
	image := codec__at(k, "image", codec__block_markup__image(k))
	figure := codec__at(k, "figure", codec__block_markup__figure(k))
	para := codec__transmute(k, Block_Markup__Paragraph, codec__para(k, imarkup))
	table := codec__at(k, "table", codec__table(k))
	// Block_Markup__List,

	return codec__sum(
		k,
		Block_Markup__Atom,
		codec__space(k, Block_Markup__Atom),
		codec__variant(k, Block_Markup__Atom, blockquote),
		codec__variant(k, Block_Markup__Atom, description),
		codec__variant(k, Block_Markup__Atom, table_of_contents),
		codec__variant(k, Block_Markup__Atom, thematic_break),
		codec__variant(k, Block_Markup__Atom, image),
		codec__variant(k, Block_Markup__Atom, figure),
		codec__variant(k, Block_Markup__Atom, para),
		codec__variant(k, Block_Markup__Atom, table),
	)
}

codec__block_markup :: proc(kit: ^Codec_Kit) -> Typed_Codec(Block_Markup) {
	return codec__memo(
		kit,
		Block_Markup,
		"block_markup",
		proc(kit: ^Codec_Kit) -> Typed_Codec(Block_Markup) {
			return codec__transmute(
				kit,
				Block_Markup,
				codec__spaced_exparr(kit, codec__block_markup__atom(kit)),
			)
		},
	)
}
// }}}

// {{{ Tables
Table__Cell :: struct {
	content: Inline_Markup,
}

Table__Row :: struct {
	cells: Exparr(Table__Cell),
}

Table :: struct {
	caption: Inline_Markup,
	header:  Table__Row,
	rows:    Exparr(Table__Row),
}
// }}}
// {{{ Codecs
@(private = "file")
codec__table__cell :: proc(k: ^Codec_Kit) -> Typed_Codec(Table__Cell) {
	return codec__field(k, "content", Table__Cell, codec__inline_markup(k))
}

@(private = "file")
codec__table__row :: proc(k: ^Codec_Kit) -> Typed_Codec(Table__Row) {
	cell := codec__at(k, "cell", codec__table__cell(k))
	return codec__field(k, "cells", Table__Row, codec__spaced_exparr(k, cell))
}

@(private = "file")
codec__table :: proc(k: ^Codec_Kit) -> Typed_Codec(Table) {
	row := codec__table__row(k)
	caption := codec__field(k, "caption", Table, codec__inline_markup(k))
	header := codec__field_at(k, "header", Table, codec__once(k, row))
	rows := codec__field(k, "rows", Table, codec__exparr(k, codec__at(k, "row", row)))

	return codec__loop(k, codec__sum(k, Table, caption, header, rows))
}
// }}}

// {{{ Pages
Linkdef :: struct {
	id:     string,
	target: string,
	label:  Inline_Markup,
}

Page :: struct {
	links: Exparr(Linkdef),
}
// }}}
