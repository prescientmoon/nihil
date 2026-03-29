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

  inner_codec: ^Codec,
  outer_codec: ^Codec,

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

codec__make :: proc(kit: ^Codec_Kit, ty: typeid) -> ^Codec {
	kit.site.statistics.codecs += 1
	codec, err := new(Codec, kit.forever)
	codec.type = ty
	log.assert(err == nil)
	return codec
}

codec__space :: proc(kit: ^Codec_Kit, box: any) -> ^Codec {
	codec := codec__make(kit, box.id)
	codec.data = Codec__Space(mem__reflect__clone(box.id, box.data, kit.forever))

	return codec
}

codec__const :: proc(kit: ^Codec_Kit, name: string, box: any) -> ^Codec {
	codec := codec__make(kit, box.id)
	codec.data = Codec__Constant {
		name  = name,
		value = mem__reflect__clone(box.id, box.data, kit.forever),
	}

	return codec
}

codec__string :: proc(kit: ^Codec_Kit) -> ^Codec {
	codec := codec__make(kit, string)
	codec.data = Codec__Text{}
	return codec
}

codec__raw :: proc(kit: ^Codec_Kit) -> ^Codec {
	codec := codec__make(kit, string)
	codec.data = Codec__Raw{}
	return codec
}

codec__loop :: proc(kit: ^Codec_Kit, inner: ^Codec) -> ^Codec {
	codec := codec__make(kit, inner.type)
	codec.data = Codec__Loop(inner)
	return codec
}

codec__at :: proc(
  kit: ^Codec_Kit, name: string, inner: ^Codec, flags: Apparition_Flags = {}
) -> ^Codec {
	codec := codec__make(kit, inner.type)
	codec.data = Codec__At { name, inner }
  return codec__tracked(kit, codec, name, flags)
}

codec__focus :: proc(
	kit: ^Codec_Kit,
  outer: typeid,
	inner: ^Codec,
	lens: proc(kit: ^Lens_Kit),
	user_data: any = nil,
  scratch := false,
) -> ^Codec {
  user_data_ptr: rawptr
  if user_data != nil {
    user_data_ptr = mem__reflect__clone(
      user_data.id, user_data.data, kit.forever
    )
  }

	codec := codec__make(kit, outer)
	codec.data = Codec__Focus {
		inner     = inner,
		lens      = lens,
		user_data = user_data_ptr,
    scratch   = scratch,
	}

	return codec
}

codec__transmute :: proc(kit: ^Codec_Kit, to: typeid, inner: ^Codec) -> ^Codec {
  log.assert(mem__same_layout(to, inner.type))
	return inner
}

@(private="file")
get_field_of_type :: proc(
  outer, inner: typeid, name: string, loc := #caller_location
) -> reflect.Struct_Field {
	field := reflect.struct_field_by_name(outer, name)

	log.assertf(
    field != {}, 
    "Field %v for type %v not found.",
    name,
    outer,
    loc=loc
  )

  log.assertf(
    mem__same_layout(field.type.id, inner),
		"Type missmatch for field %v of %v: expected %v, got %v instead.",
		name,
		outer,
		inner,
		field.type,
    loc=loc
	)

  return field
}

// This could technically be implemented in terms of "codec__ref_map", but
// that'd be needlessly wasteful, since we'd need the additional layer of
// indirection that is the saved map.
codec__field :: proc(
	kit: ^Codec_Kit,
	name: string,
	outer: typeid,
	inner: ^Codec,
  flags: Apparition_Flags = {}
) -> ^Codec {
  User_Data :: struct {
    offset: uintptr,
    size:   int,
  }

  data := User_Data {
    offset = get_field_of_type(outer, inner.type, name).offset,
    size   = reflect.size_of_typeid(inner.type)
  }

  lens :: proc(kit: ^Lens_Kit) {
    data := cast(^User_Data)kit.user_data
    field := mem__offset(kit.outer, data.offset)
    switch kit.mode {
    case .Project: mem.copy(kit.inner, field, data.size)
    case .Inject:  mem.copy(field, kit.inner, data.size)
    }
  }

  codec := codec__focus(kit, outer, inner, lens, data)
  return codec__tracked(kit, codec, name, flags)
}

codec__ref :: proc(kit: ^Codec_Kit, inner: ^Codec) -> ^Codec {
  lens :: proc(kit: ^Lens_Kit) {
    layout := reflect__layout(kit.inner_codec.type)
    as_ptr := cast(^rawptr)kit.outer

    switch kit.mode {
    case .Project: 
      if as_ptr^ == nil do as_ptr^ = mem__alloc(layout, kit.allocator)
      mem.copy(kit.inner, as_ptr^, int(layout.size))
    case .Inject: 
      log.assert(as_ptr^ != nil)
      mem.copy(as_ptr^, kit.inner, int(layout.size))
    }
  }

	return codec__focus(kit, rawptr, inner, lens)
}

@(private="file")
codec__exparr_elem :: proc(
  kit: ^Codec_Kit, inner: ^Codec, FCE: uint
) -> ^Codec {
  User_Data :: struct { fce: uint }
  data := User_Data { FCE }
  lens :: proc(kit: ^Lens_Kit) {
    if kit.mode == .Inject {
      data  := cast(^User_Data)kit.user_data
      outer := cast(^Exparr__Repr)kit.outer
      outer.allocator = kit.allocator
      layout := reflect__layout(kit.inner_codec.type)
      ptr := push(outer, data.fce, layout)
      mem.copy(ptr, kit.inner, int(layout.size))
    }
  }

	return codec__focus(kit, Exparr__Repr, inner, lens, data)
}

codec__exparr :: proc(
  kit: ^Codec_Kit, inner: ^Codec, FCE: uint = EXPARR__DEFAULT_FCE
) -> ^Codec {
	return codec__loop(kit, codec__exparr_elem(kit, inner, FCE))
}

// Throw away any value produced by the inner codec.
codec__forget :: proc(kit: ^Codec_Kit, ty: typeid, inner: ^Codec) -> ^Codec {
  noop :: proc(kit: ^Lens_Kit) {
    if kit.mode == .Inject do kit.ignored = true
  }

  return codec__focus(kit, ty, inner, noop, scratch = true)
}

// Similar to codec__exparr, except spaces can appear freely in between the
// elements.
codec__spaced_exparr :: proc(
  kit: ^Codec_Kit, inner: ^Codec, FCE: uint = EXPARR__DEFAULT_FCE
) -> ^Codec {
	return codec__loop(kit, codec__sum(
		kit,
		codec__forget(kit, Exparr__Repr, codec__space(kit, Unit{})),
    codec__exparr_elem(kit, inner, FCE),
	))
}

// Saves the structure inside an exparr field of the document type.
codec__remote_push :: proc(
  kit: ^Codec_Kit, field_name: string, document: typeid, inner: ^Codec,
  FCE: uint = EXPARR__DEFAULT_FCE
) -> ^Codec {
  field := get_field_of_type(
    document, Exparr__Repr, field_name
  )

  User_Data :: struct {
    offset: uintptr,
    fce:    uint,
  }

  data := User_Data { field.offset, FCE }

  lens :: proc(kit: ^Lens_Kit) {
    data := cast(^User_Data)kit.user_data
		field := cast(^Exparr__Repr)mem__offset(kit.document, data.offset)
    outer := cast(^rawptr)kit.outer
    layout := reflect__layout(kit.inner_codec.type)

    switch kit.mode {
    case .Project: 
      if outer^ == nil do outer^ = push(field, data.fce, layout)
      mem.copy(kit.inner, outer^, int(layout.size))
    case .Inject:
      log.assert(outer^ != nil)
      mem.copy(outer^, kit.inner, int(layout.size))
    }
  }

	return codec__focus(kit, rawptr, inner, lens, data)
}

codec__seq :: proc(kit: ^Codec_Kit, codecs: ..^Codec) -> ^Codec {
  log.assert(len(codecs) > 0, "Codec sequences cannot be empty")
  ty := codecs[0].type
  for codec in codecs[1:] do log.assertf(
    ty == codec.type, "Missmatched codec types: %v and %v", ty, codec.type,
  )

	codec := codec__make(kit, ty)
  allocator := kit.forever
	slice := make_slice([]Codec, len(codecs), allocator)
	for c, i in codecs do slice[i] = c^
	codec.data = Codec__Seq(slice)
	return codec
}

codec__sum :: proc(kit: ^Codec_Kit, codecs: ..^Codec) -> ^Codec {
  log.assert(len(codecs) > 0, "Codec sums cannot be empty")
  ty := codecs[0].type
  for codec in codecs[1:] do log.assertf(
    ty == codec.type, "Missmatched codec types: %v and %v", ty, codec.type,
  )

	codec := codec__make(kit, ty)
	slice := make_slice([]Codec, len(codecs), kit.forever)
	for c, i in codecs do slice[i] = c^
	codec.data = Codec__Sum(slice)
	return codec
}

Codec__Variant :: struct {
  type:  typeid,
  codec: ^Codec,
}

codec__union :: proc(
  kit: ^Codec_Kit, outer: typeid, variants: ..Codec__Variant
) -> ^Codec {
  lens :: proc(kit: ^Lens_Kit) {
    type := mem.reinterpret_copy(typeid, kit.user_data)

    // NOTE: we could store the offsets & tags in the user data for better perf
    inner := any{kit.inner, type}
    outer := any{kit.outer, kit.outer_codec.type}

    switch kit.mode {
    case .Project:
      variant := reflect.get_union_variant(outer)
      if variant.id == inner.id {
        size := reflect.size_of_typeid(type)
        mem.copy(kit.inner, variant.data, size)
      }
    case .Inject:
      reflect.set_union_value(outer, inner)
    }
  }

	codec := codec__make(kit, outer)
	slice := make_slice([]Codec, len(variants), kit.forever)
	for variant, i in variants {
    log.assert(mem__same_layout(variant.type, variant.codec.type))
    slice[i] = codec__focus(kit, outer, variant.codec, lens, variant.type)^
  }

	codec.data = Codec__Sum(slice)
	return codec
}

codec__memo :: proc(
	kit: ^Codec_Kit,
	name: string,
  ty: typeid,
	mk_inner: proc(kit: ^Codec_Kit) -> ^Codec,
) -> ^Codec {
  for iter := iter__mk(kit.memoized); entry in iter__next(&iter) {
		(entry.name == name) or_continue
		(entry.codec.type == ty) or_continue
		return entry.codec
	}

	lie := codec__make(kit, ty)
	push(&kit.memoized, Memoized_Codec{name = name, codec = lie})
	lie^ = mk_inner(kit)^
	return lie
}

// Wrapper around codec__transmute and codec__at.
codec__trans_at :: proc(
	kit: ^Codec_Kit,
	at: string,
	to: typeid,
	inner: ^Codec,
  outer_flags: Apparition_Flags = {},
  inner_flags: Apparition_Flags = ONCE
) -> ^Codec {
  transmuted := codec__transmute(kit, to, inner)
  intermediate := codec__tracked(kit, transmuted, at, inner_flags)
	return codec__at(kit, at, intermediate, outer_flags)
}

// Wrapper around codec__field and codec__at.
codec__field_at :: proc(
	kit: ^Codec_Kit,
	at: string,
	to: typeid,
	inner: ^Codec,
  outer_flags: Apparition_Flags = {},
  inner_flags: Apparition_Flags = ONCE
) -> ^Codec {
  intermediate := codec__field(kit, at, to, inner, inner_flags)
	return codec__at(kit, at, intermediate, outer_flags)
}

codec__tracked :: proc(
	kit: ^Codec_Kit,
	inner: ^Codec,
  name: string,
  flags: Apparition_Flags
) -> ^Codec {
  if flags == {} do return inner

	codec := codec__make(kit, inner.type)
	codec.data = Codec__Tracked {
		inner = inner,
    name  = name,
    flags = flags,
	}

	return codec
}

codec__para :: proc(kit: ^Codec_Kit, inner: ^Codec) -> ^Codec {
	codec := codec__make(kit, inner.type)
	codec.data = Codec__Paragraph(inner)
	return codec
}

codec__flag :: proc(kit: ^Codec_Kit, name: string) -> ^Codec {
  return codec__tracked(kit, codec__const(kit, name, true), name, UNIQUE)
}

codec__flag_at :: proc(
  kit: ^Codec_Kit, at: string, ty: typeid
) -> ^Codec {
  return codec__field(kit, at, ty, codec__flag(kit, at))
}

// Endows a codec with the ability to keep track of the location of its
// apparition. The location is stored at the given field.
codec__loc :: proc(kit: ^Codec_Kit, inner: ^Codec, at := "loc") -> ^Codec {
  User_Data :: struct {
    offset: uintptr,
    size:   int,
  }

  data := User_Data {
	  offset = get_field_of_type(inner.type, Source_Loc, at).offset,
    size   = reflect.size_of_typeid(inner.type),
  }

  lens :: proc(kit: ^Lens_Kit) {
    data := cast(^User_Data)kit.user_data
    switch kit.mode {
    case .Project: 
      loc := cast(^Source_Loc)mem__offset(kit.outer, data.offset)
      loc^ = kit.surrounded_at
      mem.copy(kit.inner, kit.outer, data.size)
    case .Inject:
      mem.copy(kit.outer, kit.inner, data.size)
    }
  }

	return codec__focus(kit, inner.type, inner, lens, data)
}
// }}}
