package anima

import "base:intrinsics"
import "base:runtime"
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
Codec__Paragraph :: distinct ^Codec // Terminated by two consecutive newlines
Codec__Sum :: distinct []Codec
Codec__Focus :: struct {
	inner:     ^Codec,
	user_data: rawptr,
	lens:      #type proc(kit: Lens_Kit),
}

Lens_Kit :: struct{
  allocator: mem.Allocator,
	outer:     rawptr,
  inner:     rawptr,
  user_data: rawptr,
  document:  rawptr,
  // Inject  => set the focus of the outer pointer to the inner pointer
  // Project => set the inner pointer to the focus of the outer pointer
	mode:      enum { Inject, Project },
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
		Codec__Paragraph,
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
	statistics:  ^Statistics,
}

codec__mk_kit :: proc(kit: ^Codec_Kit, statistics: ^Statistics) {
	kit.statistics = statistics
	err := virtual.arena_init_static(&kit.codec_arena)
	assert(err == nil)
	err = virtual.arena_init_static(&kit.memo_arena)
	assert(err == nil)
	kit.memoized.allocator = virtual.arena_allocator(&kit.memo_arena)
}

codec__make :: proc(kit: ^Codec_Kit, $T: typeid) -> Typed_Codec(T) {
	kit.statistics.codecs += 1
	codec, err := new(Codec, virtual.arena_allocator(&kit.codec_arena))
	codec.type = T
	assert(err == nil)
	return {codec}
}

codec__space :: proc(kit: ^Codec_Kit, $T: typeid) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
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
  $Outer: typeid,
	inner: Typed_Codec($Inner),
	lens: proc(kit: Lens_Kit),
	user_data: rawptr = nil,
) -> Typed_Codec(Outer) {
	codec := codec__make(kit, Outer)
	codec.data = Codec__Focus {
		inner     = inner,
		lens      = lens,
		user_data = user_data,
	}

	return codec
}

codec__transmute :: proc(
	kit: ^Codec_Kit,
	$To: typeid,
	inner: Typed_Codec($From),
) -> Typed_Codec(To) {
  #assert(size_of(To)  == size_of(From))
  #assert(align_of(To) == align_of(From))

  lens :: proc(kit: Lens_Kit) {
    switch kit.mode {
    case .Project: mem.copy(kit.inner, kit.outer, size_of(From))
    case .Inject:  mem.copy(kit.outer, kit.inner, size_of(From))
    }
  }

	return codec__focus(kit, To, inner, lens)
}

codec__ref_map :: proc(
	kit: ^Codec_Kit,
	inner: Typed_Codec($Inner),
	f: proc(outer: ^$Outer) -> ^Inner,
) -> Typed_Codec(Outer) {
  lens :: proc(kit: Lens_Kit) {
		f := cast(proc(outer: rawptr) -> rawptr)kit.user_data
    mapped := f(kit.outer)
    switch kit.mode {
    case .Project: mem.copy(kit.inner, mapped, size_of(Inner))
    case .Inject:  mem.copy(mapped, kit.inner, size_of(Inner))
    }
  }

	return codec__focus(kit, Outer, inner, lens, cast(rawptr)f)
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

	log.assertf(
    field != {}, 
    "Field %v for type %v not found.",
    name,
    typeid_of(Outer)
  )

	log.assertf(
		field.type.id == Inner,
		"Type missmatch for field %v of %v: expected %v, got %v instead.",
		name,
		typeid_of(Outer),
		typeid_of(Inner),
		field.type,
	)

  lens :: proc(kit: Lens_Kit) {
		field := cast(rawptr)(cast(uintptr)kit.outer + cast(uintptr)kit.user_data)
    switch kit.mode {
    case .Project: mem.copy(kit.inner, field, size_of(Inner))
    case .Inject:  mem.copy(field, kit.inner, size_of(Inner))
    }
  }

	return codec__focus(kit, Outer, inner, lens, cast(rawptr)field.offset)
}

codec__ref :: proc(kit: ^Codec_Kit, inner: Typed_Codec($T)) -> Typed_Codec(^T) {
  lens :: proc(kit: Lens_Kit) {
    as_ptr := cast(^rawptr)kit.outer

    switch kit.mode {
    case .Project: 
      if as_ptr^ == nil {
        t, err := new(T)
        assert(err == nil)
        mem.copy(as_ptr, &t, size_of(rawptr))
      }

      mem.copy(kit.inner, as_ptr^, size_of(rawptr))
    case .Inject: 
      log.assert(as_ptr^ != nil)
      mem.copy(as_ptr^, kit.inner, size_of(rawptr))
    }
  }

	return codec__focus(kit, ^T, inner, lens)
}

codec__forget :: proc(
	kit: ^Codec_Kit,
	$Outer: typeid,
	inner: Typed_Codec($Inner),
) -> Typed_Codec(Outer) {
	noop :: proc(kit: Lens_Kit) {}
	return codec__focus(kit, Outer, inner, noop)
}

codec__exparr :: proc(kit: ^Codec_Kit, inner: Typed_Codec($T)) -> Typed_Codec(Exparr(T)) {
  lens :: proc(kit: Lens_Kit) {
    if kit.mode == .Inject {
      outer := cast(^Exparr(T))kit.outer
      inner := cast(^T)kit.inner
      if outer.allocator == {} do outer.allocator = kit.allocator
      exparr__push(outer, inner^)
    }
  }

	return codec__loop(kit, codec__focus(kit, Exparr(T), inner, lens))
}

// Similar to codec__exparr, except spaces can appear freely in between the
// elements.
codec__spaced_exparr :: proc(kit: ^Codec_Kit, inner: Typed_Codec($T)) -> Typed_Codec(Exparr(T)) {
  lens :: proc(kit: Lens_Kit) {
    if kit.mode == .Inject {
      outer := cast(^Exparr(T))kit.outer
      inner := cast(^Maybe(T))kit.inner
      if outer.allocator == {} do outer.allocator = kit.allocator
      if value, ok := inner.(T); ok do exparr__push(outer, value)
    }
  }

	inner_sum := codec__sum(
		kit,
		Maybe(T),
		codec__variant(kit, Maybe(T), inner),
		codec__space(kit, Maybe(T)),
	)

	return codec__loop(kit, codec__focus(kit, Exparr(T), inner_sum, lens))
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
  lens :: proc(kit: Lens_Kit) {
    outer := cast(^Union)kit.outer
    inner := cast(^Inner)kit.inner
    switch kit.mode {
    case .Project:
      if v, ok := outer^.(Inner); ok do inner^ = v
    case .Inject:
      outer^ = Union(inner^)
    }
  }

	return codec__focus(kit, Union, inner, lens)
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
	at: string,
	$To: typeid,
	inner: Typed_Codec($From),
) -> Typed_Codec(To) {
	return codec__at(kit, at, codec__transmute(kit, To, inner))
}

// Wrapper around codec__field and codec__at.
codec__field_at :: proc(
	kit: ^Codec_Kit,
	at: string,
	$To: typeid,
	inner: Typed_Codec($From),
) -> Typed_Codec(To) {
	return codec__at(kit, at, codec__field(kit, at, To, inner))
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

codec__para :: proc(kit: ^Codec_Kit, inner: Typed_Codec($T)) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
	codec.data = Codec__Paragraph(inner.codec)
	return codec
}
// }}}
