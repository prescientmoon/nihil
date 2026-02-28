package anima

import "core:mem"

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
