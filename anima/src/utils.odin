// These will get moved to my "common odin stuff" library, once I set that up
// for myself.
package anima

import "core:container/small_array"
import "core:mem"
import "core:strings"
import "core:time"

// Similar to the standard library's "new", except the type of the allocation
// need not be known at compile time.
mem__reflected_new :: proc(type: typeid, allocator: mem.Allocator) -> rawptr {
	if ti := type_info_of(type); ti != nil {
		ptr, err := mem.alloc(
      size = ti.size,
      alignment = ti.align,
      allocator = allocator
    )

		assert(err == nil)
		return ptr
	}

	return nil
}

mem__offset :: proc(ptr: rawptr, offset: uintptr) -> rawptr {
  return rawptr(uintptr(ptr) + offset)
}

mem__nz :: mem__non_zero
mem__non_zero :: proc(v: $T) -> bool { return !mem__is_zero(v) }

mem__iz :: mem__is_zero
mem__is_zero :: proc(v: $T) -> bool {
  v := v
  return mem.check_zero(mem.ptr_to_bytes(&v))
}

// This doesn't really belong here, yet I have nowhere else to place it :3
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

strings__fixed_builder :: proc(
  size: uint, alloc: mem.Allocator
) -> (builder: strings.Builder) {
  builder = strings.builder_make_len_cap(0, int(size), alloc)
  builder.buf.allocator = mem.panic_allocator() // No more growth!
  return builder
}
