package anima

import "base:intrinsics"
import "base:runtime"
import "core:mem"
import "core:log"

// An array that allocates chunks in exponentially increasing sizes. As a
// result, pointers to elements are guaranteed to remain stable. Moreover,
// acceses remain relatively fast since the exact chunk & index computations
// can be performed via bit-shifts.
//
// The first chunk will have size 2^first_chunk_exp. The exponent will default
// to 3, i.e. a first chunk of 8 elements.
//
// Memory-wise, an empty exparr currently eats up 40B. Getting it lower isn't
// possible directly. Still, we could get the usage sites lower by introducing
// an Expslice type and using that everywhere where the allocator is "implied".
// Such a slice would only need to eat up 16B in its simplest form. If we want
// to keep the capacity of the chunk slice around as well (thus enabling us to
// turn an Expslice back into an Exparr by only providing back the allocator),
// then that'd require 24B in total. Unfortunately, this would require
// significant restructuring of a lot of the existing codebase, so I don't think
// I'm going to do it.
//
// Another possible safe-saving measure would be to make all the markup-related
// data structures set the FCE parameter to 1 instead of 3. I might in fact do
// this globally myself, even though a default of 3 "feels" more correct (this
// reduces the memory usage by about 10% when running anima on the few files I
// have).
//
// These sizes are small in the grand scheme of things, but they can quickly add
// up when used inside union branches for the various markup trees since, unless
// boxed, padding gets introduced for the surrounding branches as well.
Exparr :: struct($V: typeid, $first_chunk_exp: uint = 3) {
	allocator: runtime.Allocator,

  // A possible simplification is just imposing a max chunk-count (perhaps 30?),
	// and using a fixed-size array here. Since the chunk size grows
	// exponentially, the number of chunks remains quite bounded. For example, the
	// 30th chunk will already occupy 1GiB of memory (assuming each element only
	// takes up a single byte, and we start with first_chunk_exp = 0), a size
	// we should never really hit in practice. Oh well, for now a dynamic array
	// will have to do...
	chunks:    [][^]V,
	len:       uint,
}

@(private = "file")
exprarr__destructure_ix :: proc(
  $FCE: uint, #any_int ix: uint
) -> (chunk: uint, local_ix: uint) {
	total_bits :: uint(8 * size_of(uint))
	ghost_chunk :: 1 << FCE
	adjusted_ix := ix + ghost_chunk
	chunk = total_bits - 1 - FCE - intrinsics.count_leading_zeros(adjusted_ix)
	mask: uint = (ghost_chunk << chunk) - 1
	local_ix = adjusted_ix & mask
	return chunk, local_ix
}

exparr__get :: proc(
  exparr: Exparr($V, $FCE), #any_int ix: uint, loc := #caller_location
) -> ^V {
	log.assert(ix < exparr.len, loc = loc)
	chunk, local_ix := exprarr__destructure_ix(FCE, ix)
	return &exparr.chunks[chunk][local_ix]
}

exparr__push :: proc(
  exparr: ^Exparr($V, $FCE), element: V, loc := #caller_location
) -> ^V {
	log.assert(exparr.allocator != {}, loc=loc)
	chunk, lix := exprarr__destructure_ix(FCE, exparr.len)

	// Grow chunk array
	if chunk >= len(exparr.chunks) {
    data, length := mem.slice_to_components(exparr.chunks)
    new_length := max(1, 2 * length)
    stride := max(size_of([^]V), align_of([^]V))

    raw, err := mem.resize(
      data,
      stride * length,
      stride * new_length,
      align_of([^]V),
      exparr.allocator,
    )

		log.assertf(err == nil, "Failed to expand exponential array: %v", err)
		exparr.chunks = mem.slice_ptr(cast(^[^]V)raw, new_length)
  }

  // Commit a new multipointer to the chunk array
  if exparr.chunks[chunk] == nil {
		multiptr, err := mem.make_multi_pointer(
			[^]V,
			1 << (FCE + chunk),
			allocator = exparr.allocator,
		)
		log.assertf(err == nil, "Failed to expand exponential array: %v", err)
    exparr.chunks[chunk] = multiptr
  }

	ptr := &exparr.chunks[chunk][lix]
	ptr^ = element
	exparr.len += 1
	return ptr
}

// NOTE: this will invalidate pointers to the elements!
exparr__reverse :: proc(exparr: Exparr($V, $FCE)) {
	for i in 0 ..< exparr.len / 2 {
		xp := exparr__get(exparr, i)
		yp := exparr__get(exparr, exparr.len - i - 1)
		yv := yp^
		yp^ = xp^
		xp^ = yv
	}
}

// {{{ Value iterator
Exparr__Iter :: struct($T: typeid, $FCE: uint) {
  exparr: Exparr(T, FCE),
  index:  uint,
}

exparr__iter__mk :: proc(exparr: Exparr($V, $FCE)) -> Exparr__Iter(V, FCE) {
  return { exparr = exparr }
}

exparr__iter__next :: proc(
  iter: ^Exparr__Iter($V, $FCE)
) -> (v: ^V, ix: uint, ok: bool) {
  (iter.index < iter.exparr.len) or_return
  defer iter.index += 1
  return exparr__get(iter.exparr, iter.index), iter.index, true
}
// }}}
