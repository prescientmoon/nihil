// These will get moved to my "common odin stuff" library, once I set that up
// for myself.
package anima

import "base:runtime"
import "core:log"
import "core:mem"
import "core:reflect"
import "core:strings"
import "core:time"

// {{{ Reflect
reflect__type_info_of :: proc(type: typeid) -> ^reflect.Type_Info {
	ti := type_info_of(type)
  log.assertf(ti != nil, "No type info for %v", type)
  return ti
}

// Runtime verion of `mem__layout`.
reflect__layout :: proc(type: typeid) -> Layout {
  ti := reflect__type_info_of(type)
  return { size = uint(ti.size), align = uint(ti.align) }
}
// }}}
// {{{ Layout
Layout :: struct { size, align: uint }

layout__stride :: proc(layout: Layout) -> uint {
  return max(layout.size, layout.align),
}

layout__array :: proc(layout: Layout, #any_int count: uint) -> Layout {
  return {
    size  = layout__stride(layout) * count,
    align = layout.align,
  }
}
// }}}
// {{{ Memory
mem__layout :: proc($type: typeid) -> Layout {
  return { size = size_of(type), align = align_of(type) }
}

mem__alloc :: proc(
  layout: Layout,
  allocator: mem.Allocator,
  loc := #caller_location,
) -> rawptr {
  ptr, err := mem.alloc(
    size      = int(layout.size),
    alignment = int(layout.align),
    allocator = allocator,
    loc       = loc,
  )

  log.assert(err == nil, "Allocation failure", loc = loc)
  return ptr
}

mem__resize :: proc(
  ptr: rawptr,
  from, into: Layout,
  allocator: mem.Allocator,
  loc := #caller_location,
) -> rawptr {
  log.assert(from.align == into.align, loc = loc)
  ptr, err := mem.resize(
    ptr       = ptr,
    old_size  = int(from.size),
    new_size  = int(into.size),
    alignment = int(from.align),
    allocator = allocator,
    loc       = loc,
  )

  log.assert(err == nil, "Allocation failure", loc = loc)
  return ptr
}

// Runtime verion of `new`.
mem__reflect__new :: proc(type: typeid, allocator: mem.Allocator) -> rawptr {
  return mem__alloc(reflect__layout(type), allocator)
}

// Runtime verion of `new_clone`.
mem__reflect__clone :: proc(
  type: typeid, data: rawptr, allocator: mem.Allocator
) -> rawptr {
  layout := reflect__layout(type)
  ptr := mem__alloc(layout, allocator)
  mem.copy(ptr, data, int(layout.size))
  return ptr
}

mem__offset :: proc(ptr: rawptr, #any_int offset: uintptr) -> rawptr {
  return rawptr(uintptr(ptr) + offset)
}

mem__index :: proc(
  ptr: rawptr, layout: Layout, #any_int index: uint
) -> rawptr {
  return mem__offset(ptr, layout__stride(layout) * index)
}

mem__non_zero :: proc(v: $T) -> bool {
  return !mem__is_zero(v)
}

mem__is_zero :: proc(v: $T) -> bool {
  v := v
  return mem.check_zero(mem.ptr_to_bytes(&v))
}

mem__same_layout :: proc(a, b: typeid) -> bool {
  la := reflect__layout(a)
  lb := reflect__layout(b)
  return la == lb
}
// }}}
// {{{ Strings
strings__fixed_builder :: proc(
  size: uint, alloc: mem.Allocator
) -> (builder: strings.Builder) {
  builder = strings.builder_make_len_cap(0, int(size), alloc)
  builder.buf.allocator = mem.panic_allocator() // No more growth!
  return builder
}
// }}}
// {{{ Time
time__max :: proc(a, b: time.Time) -> time.Time {
  return time.Time{max(a._nsec, b._nsec)}
}
// }}}
// {{{ Containers
// These will eventually be handled by my pre-processor
iter__mk :: proc {
  exparr__iter__mk,
}

iter__next :: proc {
  exparr__iter__next,
}

fcdyn__push :: proc(fcdyn: ^[dynamic; $N]$E, e: E) {
  log.assert(1 == runtime.append(fcdyn, e))
}

// Push a values onto the given structure. Panics when out of space.
push :: proc {
  exparr__push,
  exparr__repr__push,
  fcdyn__push,
}

get :: proc {
  exparr__get,
  exparr__repr__get,
}
// }}}

Unit :: struct {}
