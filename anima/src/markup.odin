package anima


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
codec__inline_markup__space :: proc(kit: ^Codec_Kit) -> Typed_Codec(Inline_Markup__Space) {
	return codec__transmute(kit, Inline_Markup__Space, codec__space(kit))
}

codec__inline_markup__text :: proc(kit: ^Codec_Kit) -> Typed_Codec(Inline_Markup__Text) {
	string := codec__string(kit)
	return codec__transmute(kit, Inline_Markup__Text, codec__string(kit))
}

codec__inline_markup__ellipsis :: proc(kit: ^Codec_Kit) -> Typed_Codec(Inline_Markup__Ellipsis) {
	return codec__constant(kit, Inline_Markup__Ellipsis, "...", Inline_Markup__Ellipsis{})
}

codec__inline_markup__emph :: proc(kit: ^Codec_Kit) -> Typed_Codec(Inline_Markup__Emph) {
	return codec__at(
		kit,
		"*",
		codec__transmute(kit, Inline_Markup__Emph, codec__inline_markup(kit)),
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

codec__inline_markup__atom :: proc(kit: ^Codec_Kit) -> Typed_Codec(Inline_Markup__Atom) {
	return codec__sum(
		kit,
		Inline_Markup__Atom,
		codec__variant(kit, Inline_Markup__Atom, codec__inline_markup__space(kit)),
		codec__variant(kit, Inline_Markup__Atom, codec__inline_markup__text(kit)),
		codec__variant(kit, Inline_Markup__Atom, codec__inline_markup__ellipsis(kit)),
		codec__variant(kit, Inline_Markup__Atom, codec__inline_markup__emph(kit)),
	)
}
// }}}
