// These will get moved to my "common odin stuff" library, once I set that up
// for myself.
package anima

import "base:runtime"
import "core:log"
import "core:mem"
import "core:reflect"
import "core:strings"
import "core:time"

reflect__type_info_of :: proc(type: typeid) -> ^reflect.Type_Info {
	ti := type_info_of(type)
  log.assertf(ti != nil, "No type info for %v", type)
  return ti
}

// Similar to the standard library's "new", except the type of the allocation
// need not be known at compile time.
mem__reflected_new :: proc(type: typeid, allocator: mem.Allocator) -> rawptr {
	ti := reflect__type_info_of(type)
  log.assertf(ti != nil, "No type info for %v", type)

  ptr, err := mem.alloc(
    size = ti.size,
    alignment = ti.align,
    allocator = allocator
  )

  log.assert(err == nil)
  return ptr
}

// Similar to the standard library's "new_clone", except the type of the
// allocation need not be known at compile time.
mem__reflected_clone :: proc(
  type: typeid, data: rawptr, allocator: mem.Allocator
) -> rawptr {
	ti := type_info_of(type)
  log.assertf(ti != nil, "No type info for %v", type)

  ptr, err := mem.alloc(
    size = ti.size,
    alignment = ti.align,
    allocator = allocator
  )

  log.assert(err == nil)
  mem.copy(ptr, data, ti.size)
  return ptr
}

mem__offset :: proc(ptr: rawptr, #any_int offset: uintptr) -> rawptr {
  return rawptr(uintptr(ptr) + offset)
}

mem__nz :: mem__non_zero
mem__non_zero :: proc(v: $T) -> bool { return !mem__is_zero(v) }

mem__iz :: mem__is_zero
mem__is_zero :: proc(v: $T) -> bool {
  v := v
  return mem.check_zero(mem.ptr_to_bytes(&v))
}

strings__fixed_builder :: proc(
  size: uint, alloc: mem.Allocator
) -> (builder: strings.Builder) {
  builder = strings.builder_make_len_cap(0, int(size), alloc)
  builder.buf.allocator = mem.panic_allocator() // No more growth!
  return builder
}

Unit :: struct {}

time__max :: proc(a, b: time.Time) -> time.Time {
  return time.Time{max(a._nsec, b._nsec)}
}

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
}
