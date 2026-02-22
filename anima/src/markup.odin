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
@(private = "file")
codec__inline_markup__atom :: proc(k: ^Codec_Kit) -> Typed_Codec(Inline_Markup__Atom) {
	imarkup := codec__inline_markup(k)
	space := codec__transmute(k, Inline_Markup__Space, codec__space(k))
	text := codec__transmute(k, Inline_Markup__Text, codec__string(k))
	ellipsis := codec__constant(k, "...", Inline_Markup__Ellipsis{})
	emph := codec__trans_at(k, Inline_Markup__Emph, "_", imarkup)
	strong := codec__trans_at(k, Inline_Markup__Strong, "*", imarkup)
	strike := codec__trans_at(k, Inline_Markup__Strikethrough, "~", imarkup)
	mono := codec__trans_at(k, Inline_Markup__Mono, "`", imarkup)
	quote := codec__trans_at(k, Inline_Markup__Quote, "\"", imarkup)

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
