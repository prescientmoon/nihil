package anima

import "core:log"
import "core:mem"
import "core:mem/virtual"
import "core:reflect"

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
	inner:     ^Codec,
	project:   proc(outer: rawptr, inner: rawptr),
	inject:    proc(outer: rawptr, inner: rawptr),
	user_data: rawptr,
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
	user_data: rawptr = nil,
) -> Typed_Codec(Outer) {
	codec := codec__make(kit, Outer)
	codec.data = Codec__Focus {
		inner     = inner,
		project   = cast(proc(o: rawptr, i: rawptr))(project),
		inject    = cast(proc(o: rawptr, i: rawptr))(inject),
		user_data = user_data,
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

codec__ref_map :: proc(
	kit: ^Codec_Kit,
	inner: Typed_Codec($Inner),
	f: proc(outer: ^$Outer) -> ^Inner,
) -> Typed_Codec(Outer) {
	project :: proc(o: ^Outer, i: ^Inner) {
		f := cast(proc(outer: ^Outer) -> ^Inner)context.user_ptr
		i^ = f(o)^
	}

	inject :: proc(o: ^Outer, i: ^Inner) {
		f := cast(proc(outer: ^Outer) -> ^Inner)context.user_ptr
		f(o)^ = i^
	}

	return codec__focus(kit, inner, project, inject, cast(rawptr)f)
}

// This could technically be implemented in terms of "codec__ref_map", but
// that'd be needlessly wasteful, since we'd need the additional layer of
// indirection that is the saved map.
codec__field :: proc(
	kit: ^Codec_Kit,
	name: string,
	$Outer: typeid,
	inner: Typed_Codec($Inner),
) -> Typed_Codec(Outer) {
	field := reflect.struct_field_by_name(Outer, name)
	log.assertf(field != {}, "Field %v for type %v not found.", name, typeid_of(Outer))
	log.assertf(
		field.type.id == Inner,
		"Type missmatch for field %v of %v: expected %v, got %v instead.",
		name,
		typeid_of(Outer),
		typeid_of(Inner),
		field.type,
	)

	ref_map :: proc(o: ^Outer) -> ^Inner {
		offset := cast(uintptr)context.user_ptr
		return cast(^Inner)(cast(uintptr)o + offset)
	}

	project :: proc(o: ^Outer, i: ^Inner) {i^ = ref_map(o)^}
	inject :: proc(o: ^Outer, i: ^Inner) {ref_map(o)^ = i^}

	return codec__focus(kit, inner, project, inject, cast(rawptr)field.offset)
}

codec__ref :: proc(kit: ^Codec_Kit, inner: Typed_Codec($T)) -> Typed_Codec(^T) {
	project :: proc(o: ^^T, i: ^T) {
		if o^ == nil {
			t, err := new(T)
			assert(err == nil)
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

codec__forget :: proc(
	kit: ^Codec_Kit,
	$Outer: typeid,
	inner: Typed_Codec($Inner),
) -> Typed_Codec(Outer) {
	noop :: proc(o: ^Outer, i: ^Inner) {}
	return codec__focus(kit, inner, noop, noop)
}

codec__exparr :: proc(
	kit: ^Codec_Kit,
	inner: Typed_Codec($T),
	non_zero := false,
) -> Typed_Codec(Exparr(T)) {
	User_Data :: struct {
		non_zero: bool,
	}

	allocator := virtual.arena_allocator(&kit.codec_arena)
	user_data, err := new_clone(User_Data{non_zero}, allocator)
	log.assert(err == nil)

	project :: proc(o: ^Exparr(T), i: ^T) {}
	inject :: proc(o: ^Exparr(T), i: ^T) {
		user_data := (cast(^User_Data)context.user_ptr)^
		if o.allocator == {} do o.allocator = context.allocator
		if !user_data.non_zero || !mem.check_zero(mem.ptr_to_bytes(i)) {
			exparr__push(o, i^)
		}
	}

	return codec__loop(kit, codec__focus(kit, inner, project, inject, user_data))
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

codec__tracked :: proc(
	kit: ^Codec_Kit,
	inner: Typed_Codec($T),
	unique := false,
	required := false,
) -> Typed_Codec(T) {
	log.assert(unique || required, "A tracked codec mustn't track nothing")

	codec := codec__make(kit, T)
	codec.data = Codec__Tracked {
		inner    = inner,
		required = required,
		unique   = unique,
	}

	return codec
}

// Marks a codec as both unique and required.
codec__once :: proc(kit: ^Codec_Kit, codec: Typed_Codec($T)) -> Typed_Codec(T) {
	return codec__tracked(kit, codec, unique = true, required = true)
}
// }}}
