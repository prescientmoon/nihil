package anima

import "core:mem/virtual"

// {{{ The Codec type
Unit :: struct {}
Codec__Text :: distinct Unit
Codec__Space :: distinct Unit

Codec__Constant :: struct {
	name:  string,
	value: rawptr,
}

Codec__At :: struct {
	name:  string,
	inner: ^Codec,
}

Codec__Tracked :: struct {
	inner:    ^Codec,
	required: bool, // Must run at least once
	unique:   bool, // Cannot run more than once
}

Codec__Loop :: distinct ^Codec
Codec__Sum :: distinct []Codec
Codec__Focus :: struct {
	inner:   ^Codec,
	project: proc(outer: rawptr, inner: rawptr),
	inject:  proc(outer: rawptr, inner: rawptr),
}

Codec :: struct {
	type: typeid,
	data: union {
		Codec__Text,
		Codec__Space,
		Codec__Constant,
		Codec__At,
		Codec__Tracked,
		Codec__Sum,
		Codec__Focus,
		Codec__Loop,
	},
}
// }}}
// {{{ Typed codecs
Typed_Codec :: struct($on: typeid) {
	using codec: ^Codec,
}

Memoized_Codec :: struct {
	name:  string,
	codec: ^Codec,
}

Codec_Kit :: struct {
	codec_arena: virtual.Arena,
	memo_arena:  virtual.Arena,
	memoized:    Exparr(Memoized_Codec),
}

codec__mk_kit :: proc(kit: ^Codec_Kit) {
	err := virtual.arena_init_static(&kit.codec_arena)
	assert(err == nil)
	err = virtual.arena_init_static(&kit.memo_arena)
	assert(err == nil)
	kit.memoized.allocator = virtual.arena_allocator(&kit.memo_arena)
}

codec__make :: proc(kit: ^Codec_Kit, $T: typeid) -> Typed_Codec(T) {
	codec, err := new(Codec, virtual.arena_allocator(&kit.codec_arena))
	codec.type = T
	assert(err == nil)
	return {codec}
}

codec__space :: proc(kit: ^Codec_Kit) -> Typed_Codec(Unit) {
	codec := codec__make(kit, Unit)
	codec.data = Codec__Space{}
	return codec
}

codec__constant :: proc(kit: ^Codec_Kit, name: string, value: $T) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
	codec.data = Codec__Constant {
		name  = name,
		value = new_clone(value, virtual.arena_allocator(&kit.codec_arena)),
	}
	return codec
}

codec__string :: proc(kit: ^Codec_Kit) -> Typed_Codec(string) {
	codec := codec__make(kit, string)
	codec.data = Codec__Text{}
	return codec
}

codec__loop :: proc(kit: ^Codec_Kit, inner: Typed_Codec($T)) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
	codec.data = Codec__Loop(inner.codec)
	return codec
}

codec__at :: proc(kit: ^Codec_Kit, name: string, inner: Typed_Codec($T)) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
	codec.data = Codec__At {
		name  = name,
		inner = inner.codec,
	}

	return codec
}

codec__focus :: proc(
	kit: ^Codec_Kit,
	inner: Typed_Codec($Inner),
	project: proc(o: ^$Outer, i: ^Inner),
	inject: proc(o: ^Outer, i: ^Inner),
) -> Typed_Codec(Outer) {
	codec := codec__make(kit, Outer)
	codec.data = Codec__Focus {
		inner   = inner,
		project = cast(proc(o: rawptr, i: rawptr))(project),
		inject  = cast(proc(o: rawptr, i: rawptr))(inject),
	}

	return codec
}


codec__transmute :: proc(
	kit: ^Codec_Kit,
	$To: typeid,
	inner: Typed_Codec($From),
) -> Typed_Codec(To) where size_of(To) == size_of(From) &&
	align_of(To) == align_of(From) {

	project :: proc(o: ^To, i: ^From) {i^ = transmute(From)o^}
	inject :: proc(o: ^To, i: ^From) {o^ = transmute(To)i^}

	return codec__focus(kit, inner, project = project, inject = inject)
}

codec__ref :: proc(kit: ^Codec_Kit, inner: Typed_Codec($T)) -> Typed_Codec(^T) {
	project :: proc(o: ^^T, i: ^T) {
		if o^ == nil {
			t, err := new(T)
			assert(t == nil)
			o^ = t
		}

		i^ = o^^
	}

	inject :: proc(o: ^^T, i: ^T) {
		assert(o^ != nil)
		o^^ = i^
	}

	return codec__focus(kit, inner, project = project, inject = inject)
}

codec__exparr :: proc(kit: ^Codec_Kit, inner: Typed_Codec($T)) -> Typed_Codec(Exparr(T)) {
	project :: proc(o: ^Exparr(T), i: ^T) {}
	inject :: proc(o: ^Exparr(T), i: ^T) {
		if o.allocator == {} do o.allocator = context.allocator
		exparr__push(o, i^)
	}

	return codec__loop(kit, codec__focus(kit, inner, project = project, inject = inject))
}

codec__sum :: proc(kit: ^Codec_Kit, $T: typeid, codecs: ..Typed_Codec(T)) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
	slice := make_slice([]Codec, len(codecs), virtual.arena_allocator(&kit.codec_arena))
	for c, i in codecs do slice[i] = c.codec^

	codec.data = Codec__Sum(slice)
	return codec
}

codec__variant :: proc(
	kit: ^Codec_Kit,
	$Union: typeid,
	inner: Typed_Codec($Inner),
) -> Typed_Codec(Union) {
	project :: proc(o: ^Union, i: ^Inner) {
		if v, ok := o^.(Inner); ok {
			i^ = v
		}
	}

	inject :: proc(o: ^Union, i: ^Inner) {
		o^ = Union(i^)
	}

	return codec__focus(kit, inner, project = project, inject = inject)
}

codec__memo :: proc(
	kit: ^Codec_Kit,
	$T: typeid,
	name: string,
	mk_inner: proc(kit: ^Codec_Kit) -> Typed_Codec(T),
) -> Typed_Codec(T) {
	for i in 0 ..< kit.memoized.len {
		entry := exparr__get(kit.memoized, i)
		(entry.name == name) or_continue
		(entry.codec.type == T) or_continue
		return Typed_Codec(T){entry.codec}
	}

	lie := codec__make(kit, T)
	exparr__push(&kit.memoized, Memoized_Codec{name = name, codec = lie.codec})
	codec := mk_inner(kit)
	lie.codec^ = codec.codec^
	return lie
}

// Wrapper around codec__transmute and codec__at.
codec__trans_at :: proc(
	kit: ^Codec_Kit,
	$To: typeid,
	at: string,
	inner: Typed_Codec($From),
) -> Typed_Codec(To) {
	return codec__at(kit, at, codec__transmute(kit, To, inner))
}
// }}}
