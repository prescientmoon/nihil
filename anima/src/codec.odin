package anima

import "core:log"
import "core:fmt"
import "core:mem"
import "core:mem/virtual"
import "core:reflect"

// {{{ The Codec type
Codec__Text  :: distinct Unit
Codec__Space :: distinct rawptr
Codec__Raw   :: distinct Unit

Codec__Constant :: struct {
	name:  string,
	value: rawptr,
}

Codec__At :: struct {
	name:  string,
	inner: ^Codec,
}

// .Required => Must run at least once
// .Unique   => Cannot run more than once
Apparition_Flag  :: enum { Unique, Required }
Apparition_Flags :: bit_set[Apparition_Flag]

UNIQUE   : Apparition_Flags : { .Unique }
REQUIRED : Apparition_Flags : { .Required }
ONCE     : Apparition_Flags : REQUIRED | UNIQUE

Codec__Tracked :: struct {
	inner: ^Codec,
  name:  string, // This is useful for inclusion in error messages.
  flags: Apparition_Flags,
}

Codec__Seq       :: distinct []Codec
Codec__Sum       :: distinct []Codec
Codec__Loop      :: distinct ^Codec
Codec__Paragraph :: distinct ^Codec // Terminated by two consecutive newlines

// A lens-based constructor that allows changing the type of data the codecs are
// working on. This is the primary means of injecting custom logic into the
// parsing process.
Codec__Focus :: struct {
	inner:     ^Codec,
	user_data: rawptr,
	lens:      #type proc(kit: ^Lens_Kit),

  // When true, the children are not given access to the output arena, and are 
  // instead allocated on the dynamic stack. The stack cleanup for the children
  // will then be delayed until this lens has finished running, thus the data
  // can be processed/saved up in the output arena.
  scratch: bool,
}

Codec :: struct {
	type: typeid,
	data: union {
		Codec__Text,
		Codec__Space,
		Codec__Raw,
		Codec__Constant,
		Codec__At,
		Codec__Tracked,
		Codec__Seq,
		Codec__Sum,
		Codec__Focus,
		Codec__Loop,
		Codec__Paragraph,
	},
}

Lens_Kit :: struct{
  allocator, temp_allocator, error_allocator: mem.Allocator,

	outer:     rawptr,
  inner:     rawptr,
  document:  rawptr,
  user_data: rawptr,

  // Can be used to keep track of locations in the resulting markup.
  surrounded_at: Source_Loc,

  // Codecs can crash at any point, reporting their errors here.
  errors:    Exparr(string),

  // Inject  => set the focus of the outer pointer to the inner pointer
  // Project => set the inner pointer to the focus of the outer pointer
	mode: enum { Inject, Project },

  // While projecting, this can be used to block execution of the inner codec.
  // While injecting, this can be used to ignore consumption of trivial tokens
  // like whitespace.
  ignored: bool,
}

lens__errorf :: proc(kit: ^Lens_Kit, format: string, args: ..any) {
	msg := fmt.aprintf(format, ..args, allocator = kit.error_allocator)
	push(&kit.errors, msg)
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
  site:     ^Site,
  forever:  mem.Allocator,
	memoized: Exparr(Memoized_Codec),
}

// NOTE: allocates on the dynamic stack.
codec__kit__make :: proc(site: ^Site) -> (kit: Codec_Kit) {
  kit.site = site
	kit.memoized.allocator = site__alloc(site, .Stack)
	kit.forever = site__alloc(site, .Forever)
  return kit
}

codec__make :: proc(kit: ^Codec_Kit, $T: typeid) -> Typed_Codec(T) {
	kit.site.statistics.codecs += 1
	codec, err := new(Codec, kit.forever)
	codec.type = T
	assert(err == nil)
	return {codec}
}

codec__space :: proc(kit: ^Codec_Kit, $T: typeid, value: T) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
	codec.data = Codec__Space(
    new_clone(value, kit.forever)
  )

	return codec
}

codec__const :: proc(
  kit: ^Codec_Kit, name: string, value: $T
) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
	codec.data = Codec__Constant {
		name  = name,
		value = new_clone(value, kit.forever),
	}

	return codec
}

codec__string :: proc(kit: ^Codec_Kit) -> Typed_Codec(string) {
	codec := codec__make(kit, string)
	codec.data = Codec__Text{}
	return codec
}

codec__raw :: proc(kit: ^Codec_Kit) -> Typed_Codec(string) {
	codec := codec__make(kit, string)
	codec.data = Codec__Raw{}
	return codec
}

codec__loop :: proc(kit: ^Codec_Kit, inner: Typed_Codec($T)) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
	codec.data = Codec__Loop(inner.codec)
	return codec
}

codec__at :: proc(
  kit: ^Codec_Kit, name: string, inner: Typed_Codec($T),
  flags: Apparition_Flags = {}
) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
	codec.data = Codec__At {
		name  = name,
		inner = inner.codec,
	}

  if flags != {} {
    return codec__tracked(kit, codec, name, flags)
  } else {
    return codec
  }
}

codec__focus :: proc(
	kit: ^Codec_Kit,
  $Outer: typeid,
	inner: Typed_Codec($Inner),
	lens: proc(kit: ^Lens_Kit),
	user_data: rawptr = nil,
  scratch := false,
) -> Typed_Codec(Outer) {
	codec := codec__make(kit, Outer)
	codec.data = Codec__Focus {
		inner     = inner,
		lens      = lens,
		user_data = user_data,
    scratch   = scratch,
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

  lens :: proc(kit: ^Lens_Kit) {
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
  lens :: proc(kit: ^Lens_Kit) {
		f := cast(proc(outer: rawptr) -> rawptr)kit.user_data
    mapped := f(kit.outer)
    switch kit.mode {
    case .Project: mem.copy(kit.inner, mapped, size_of(Inner))
    case .Inject:  mem.copy(mapped, kit.inner, size_of(Inner))
    }
  }

	return codec__focus(kit, Outer, inner, lens, rawptr(f))
}

@(private="file")
get_field_of_type :: proc(
  Outer, Inner: typeid, name: string
) -> reflect.Struct_Field {
	field := reflect.struct_field_by_name(Outer, name)

	log.assertf(
    field != {}, 
    "Field %v for type %v not found.",
    name,
    Outer
  )

	log.assertf(
		field.type.id == Inner,
		"Type missmatch for field %v of %v: expected %v, got %v instead.",
		name,
		Outer,
		Inner,
		field.type,
	)

  return field
}

// This could technically be implemented in terms of "codec__ref_map", but
// that'd be needlessly wasteful, since we'd need the additional layer of
// indirection that is the saved map.
codec__field :: proc(
	kit: ^Codec_Kit,
	name: string,
	$Outer: typeid,
	inner: Typed_Codec($Inner),
  flags: Apparition_Flags = {}
) -> Typed_Codec(Outer) {
	field := get_field_of_type(Outer, Inner, name)
  lens :: proc(kit: ^Lens_Kit) {
    field := mem__offset(kit.outer, uintptr(kit.user_data))
    switch kit.mode {
    case .Project: mem.copy(kit.inner, field, size_of(Inner))
    case .Inject:  mem.copy(field, kit.inner, size_of(Inner))
    }
  }

  codec := codec__focus(kit, Outer, inner, lens, rawptr(field.offset))
  if flags == {} {
    return codec
  } else {
    return codec__tracked(kit, codec, name, flags)
  }
}

codec__ref :: proc(kit: ^Codec_Kit, inner: Typed_Codec($T)) -> Typed_Codec(^T) {
  lens :: proc(kit: ^Lens_Kit) {
    as_ptr := cast(^rawptr)kit.outer

    switch kit.mode {
    case .Project: 
      if as_ptr^ == nil {
        t, err := new(T, kit.allocator)
        log.assert(err == nil)
        as_ptr^ = t
      }

      mem.copy(kit.inner, as_ptr^, size_of(T))
    case .Inject: 
      log.assert(as_ptr^ != nil)
      mem.copy(as_ptr^, kit.inner, size_of(T))
    }
  }

	return codec__focus(kit, ^T, inner, lens)
}

codec__forget :: proc(
	kit: ^Codec_Kit,
	$Outer: typeid,
	inner: Typed_Codec($Inner),
) -> Typed_Codec(Outer) {
	noop :: proc(kit: ^Lens_Kit) {}
	return codec__focus(kit, Outer, inner, noop)
}

codec__exparr :: proc(
  kit: ^Codec_Kit, inner: Typed_Codec($T)
) -> Typed_Codec(Exparr(T)) {
  lens :: proc(kit: ^Lens_Kit) {
    if kit.mode == .Inject {
      outer := cast(^Exparr(T))kit.outer
      inner := cast(^T)kit.inner
      if outer.allocator == {} do outer.allocator = kit.allocator
      push(outer, inner^)
    }
  }

	return codec__loop(kit, codec__focus(kit, Exparr(T), inner, lens))
}

// Similar to codec__exparr, except spaces can appear freely in between the
// elements.
codec__spaced_exparr :: proc(
  kit: ^Codec_Kit, inner: Typed_Codec($T)
) -> Typed_Codec(Exparr(T)) {
  lens :: proc(kit: ^Lens_Kit) {
    if kit.mode == .Inject {
      outer := cast(^Exparr(T))kit.outer
      inner := cast(^Maybe(T))kit.inner
      if outer.allocator == {} do outer.allocator = kit.allocator
      if value, ok := inner.(T); ok do push(outer, value)
      else do kit.ignored = true
    }
  }

	inner_sum := codec__sum(
		kit,
		Maybe(T),
		codec__space(kit, Maybe(T), nil),
		codec__variant(kit, Maybe(T), inner),
	)

	return codec__loop(kit, codec__focus(kit, Exparr(T), inner_sum, lens))
}

// Saves the structure inside an exparr field of the document type.
codec__remote_push :: proc(
  kit: ^Codec_Kit, field_name: string, $Document: typeid, inner: Typed_Codec($T) 
) -> Typed_Codec(^T) {
  field := get_field_of_type(Document, Exparr(T), field_name)

  lens :: proc(kit: ^Lens_Kit) {
		field := cast(^Exparr(T))mem__offset(kit.document, uintptr(kit.user_data))
    outer := cast(^rawptr)kit.outer

    switch kit.mode {
    case .Project: 
      if outer^ == nil {
        fresh := push(field, T{})
        outer^ = fresh
      }

      mem.copy(kit.inner, outer^, size_of(T))
    case .Inject:
      log.assert(outer^ != nil)
      mem.copy(outer^, kit.inner, size_of(T))
    }
  }

	return codec__focus(kit, ^T, inner, lens, rawptr(field.offset))
}

codec__seq :: proc(
  kit: ^Codec_Kit, $T: typeid, codecs: ..Typed_Codec(T)
) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
  allocator := kit.forever
	slice := make_slice([]Codec, len(codecs), allocator)
	for c, i in codecs do slice[i] = c.codec^

	codec.data = Codec__Seq(slice)
	return codec
}

codec__sum :: proc(
  kit: ^Codec_Kit, $T: typeid, codecs: ..Typed_Codec(T)
) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
  allocator := kit.forever
	slice := make_slice([]Codec, len(codecs), allocator)
	for c, i in codecs do slice[i] = c.codec^

	codec.data = Codec__Sum(slice)
	return codec
}

codec__variant :: proc(
	kit: ^Codec_Kit,
	$Union: typeid,
	inner: Typed_Codec($Inner),
) -> Typed_Codec(Union) {
  lens :: proc(kit: ^Lens_Kit) {
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
	name: string,
	mk_inner: proc(kit: ^Codec_Kit) -> Typed_Codec($T),
) -> Typed_Codec(T) {
  for iter := iter__mk(kit.memoized); entry in iter__next(&iter) {
		(entry.name == name) or_continue
		(entry.codec.type == T) or_continue
		return Typed_Codec(T){entry.codec}
	}

	lie := codec__make(kit, T)
	push(&kit.memoized, Memoized_Codec{name = name, codec = lie.codec})
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
  outer_flags: Apparition_Flags = {},
  inner_flags: Apparition_Flags = ONCE
) -> Typed_Codec(To) {
  transmuted := codec__transmute(kit, To, inner)
  intermediate := codec__tracked(kit, transmuted, at, inner_flags)
	return codec__at(kit, at, intermediate, outer_flags)
}

// Wrapper around codec__field and codec__at.
codec__field_at :: proc(
	kit: ^Codec_Kit,
	at: string,
	$To: typeid,
	inner: Typed_Codec($From),
  outer_flags: Apparition_Flags = {},
  inner_flags: Apparition_Flags = ONCE
) -> Typed_Codec(To) {
  intermediate := codec__field(kit, at, To, inner, inner_flags)
	return codec__at(kit, at, intermediate, outer_flags)
}

codec__tracked :: proc(
	kit: ^Codec_Kit,
	inner: Typed_Codec($T),
  name: string,
  flags: Apparition_Flags
) -> Typed_Codec(T) {
  if flags == {} do return inner

	codec := codec__make(kit, T)
	codec.data = Codec__Tracked {
		inner    = inner,
    name     = name,
    flags    = flags,
	}

	return codec
}

codec__para :: proc(kit: ^Codec_Kit, inner: Typed_Codec($T)) -> Typed_Codec(T) {
	codec := codec__make(kit, T)
	codec.data = Codec__Paragraph(inner.codec)
	return codec
}

codec__flag :: proc(kit: ^Codec_Kit, name: string) -> Typed_Codec(bool) {
  return codec__tracked(kit, codec__const(kit, name, true), name, UNIQUE)
}

codec__flag_at :: proc(
  kit: ^Codec_Kit, at: string, $T: typeid
) -> Typed_Codec(T) {
  return codec__field(kit, at, T, codec__flag(kit, at))
}

// Endows a codec with the ability to keep track of the location of its
// apparition. The location is stored at the given field.
codec__loc :: proc(
  kit: ^Codec_Kit, inner: Typed_Codec($T), at := "loc"
) -> Typed_Codec(T) {
	field := get_field_of_type(T, Source_Loc, at)

  lens :: proc(kit: ^Lens_Kit) {
    switch kit.mode {
    case .Project: 
      loc := cast(^Source_Loc)mem__offset(kit.outer, uintptr(kit.user_data))
      loc^ = kit.surrounded_at

      mem.copy(kit.inner, kit.outer, size_of(T))
    case .Inject:
      mem.copy(kit.outer, kit.inner, size_of(T))
    }
  }

	return codec__focus(kit, T, inner, lens, rawptr(field.offset))
}
// }}}
