package anima

import "base:intrinsics"
import "base:runtime"
import "core:mem"

// An array that allocates chunks in exponentially increasing sizes. As a
// result, pointers to elements are guaranteed to remain stable. Moreover,
// acceses remain relatively fast since the exact chunk & index computations
// can be performed via bit-shifts.
//
// The first chunk will have size 2^first_chunk_exp. The exponent will default
// to 3, i.e. a first chunk of 8 elements.
Exparr :: struct($V: typeid, $first_chunk_exp: uint = 3) {
	allocator: runtime.Allocator,

	// Each chunk doubles the size! We don't need a full-blown dynamic array for
	// the chunks, but I'm too lazy to do things manually, so here we are :3
	//
	// A possible simplification is just imposing a max chunk-count (perhaps 30?),
	// and using a fixed-size array here. Since the chunk size grows
	// exponentially, the number of chunks remains quite bounded. For example, the
	// 30th chunk will already occupy 1GiB of memory (assuming each element only
	// takes up a single byte, and we start with first_chunk_exp = 0), a size
	// we should never really hit in practice. Oh well, for now a dynamic array
	// will have to do...
	chunks:    [dynamic][^]V,
	len:       uint,
}

@(private = "file")
exprarr_destructure_ix :: proc($FCE: uint, #any_int ix: uint) -> (chunk: uint, local_ix: uint) {
	total_bits :: uint(8 * size_of(uint))
	ghost_chunk :: 1 << FCE
	adjusted_ix := ix + ghost_chunk
	chunk = total_bits - 1 - FCE - intrinsics.count_leading_zeros(adjusted_ix)
	mask: uint = (ghost_chunk << chunk) - 1
	local_ix = adjusted_ix & mask
	return chunk, local_ix
}

exparr_get :: proc(exparr: Exparr($V, $FCE), #any_int ix: uint) -> ^V {
	assert(ix < exparr.len)
	chunk, local_ix := exprarr_destructure_ix(FCE, ix)
	return &exparr.chunks[chunk][local_ix]
}

exparr_last :: proc(exparr: Exparr($V, $FCE)) -> ^V {
	assert(exparr.len > 0)
	return exparr_get(exparr, exparr.len - 1)
}

exparr_push :: proc(exparr: ^Exparr($V, $FCE), element: V) -> ^V {
	assert(exparr.allocator != {})
	chunk, lix := exprarr_destructure_ix(FCE, exparr.len)

	// Grow
	if chunk >= len(exparr.chunks) {
		multiptr, err := mem.make_multi_pointer(
			[^]V,
			1 << (FCE + len(exparr.chunks)),
			allocator = exparr.allocator,
		)
		assert(err == nil)

		append_elem(&exparr.chunks, multiptr)
	}

	ptr := &exparr.chunks[chunk][lix]
	ptr^ = element
	exparr.len += 1
	return ptr
}

exparr_pop :: proc(exparr: ^Exparr($V, $FCE)) -> V {
	assert(exparr.len > 0)
	v := exparr_get(exparr^, exparr.len - 1)
	exparr.len -= 1
	return v^
}

// NOTE: this will invalidate pointers to the elements!
exparr_reverse :: proc(exparr: Exparr($V, $FCE)) {
	for i in 0 ..< exparr.len / 2 {
		xp := exparr_get(exparr, i)
		yp := exparr_get(exparr, exparr.len - i - 1)
		yv := yp^
		yp^ = xp^
		xp^ = yv
	}
}

exparr_push_exparr :: proc(exparr: ^Exparr($V, $FCE), elements: Exparr(V, $OFCE)) {
	for i in 0 ..< elements.len {
		exparr_push(exparr, exparr_get(elements, i)^)
	}
}
