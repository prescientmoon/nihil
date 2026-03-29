package anima

import "base:intrinsics"
import "core:mem"
import "core:log"

PLATFORM_BITS :: 8 * size_of(uint)
EXPARR__DEFAULT_FCE :: 3

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
Exparr :: struct($V: typeid, $first_chunk_exp: uint = EXPARR__DEFAULT_FCE) {
  using repr: Exparr__Repr,
}

// Untyped representation of the data held by an exponential array.
Exparr__Repr :: struct {
	allocator: mem.Allocator,
  // A possible simplification is just imposing a max chunk-count (perhaps 30?),
	// and using a fixed-size array here. Since the chunk size grows
	// exponentially, the number of chunks remains quite bounded. For example, the
	// 30th chunk will already occupy 1GiB of memory (assuming each element only
	// takes up a single byte, and we start with first_chunk_exp = 0), a size
	// we should never really hit in practice. Oh well, for now a dynamic array
	// will have to do...
	chunks:    []rawptr,
	len:       uint,
}

@(private = "file")
exprarr__destructure_ix :: proc(
  FCE: uint, #any_int ix: uint
) -> (chunk: uint, local_ix: uint) {
	ghost_chunk := uint(1 << FCE)
	adjusted_ix := ix + ghost_chunk
	chunk = PLATFORM_BITS - 1 - FCE - intrinsics.count_leading_zeros(adjusted_ix)
	mask: uint = (ghost_chunk << chunk) - 1
	local_ix = adjusted_ix & mask
	return chunk, local_ix
}

// {{{ Get particular elements
exparr__repr__get :: proc(
  exparr: Exparr__Repr,
  FCE: uint,
  layout: Layout,
  #any_int ix: uint,
  loc := #caller_location,
) -> rawptr {
	log.assert(ix < exparr.len, loc = loc)
	chunk, local_ix := exprarr__destructure_ix(FCE, ix)
	return mem__index(exparr.chunks[chunk], layout, local_ix)
}

exparr__get :: proc(
  exparr: Exparr($V, $FCE), #any_int ix: uint, loc := #caller_location
) -> ^V {
  return cast(^V)exparr__repr__get(exparr.repr, FCE, mem__layout(V), ix, loc)
}

exparr__try_last :: proc(
  x: Exparr($V, $FCE), loc := #caller_location
) -> (ptr: ^V, ok: bool) {
	(x.len > 0) or_return
  ptr = cast(^V)exparr__repr__get(x.repr, FCE, mem__layout(V), x.len - 1, loc)
  return ptr, true
}

exparr__last :: proc(x: Exparr($V, $FCE), loc := #caller_location) -> ^V {
	log.assert(x.len > 0, loc = loc)
  return cast(^V)exparr__repr__get(x.repr, FCE, mem__layout(V), x.len - 1, loc)
}
// }}}
// {{{ Push
// The untyped version of `exparr__push`. Pushes an empty element onto the
// untyped exparr representation and returns a pointer to it.
exparr__repr__push :: proc(
  exparr: ^Exparr__Repr,
  FCE: uint,
  layout: Layout,
  loc := #caller_location,
) -> rawptr {
	log.assert(exparr.allocator != {}, loc=loc)
	chunk, lix := exprarr__destructure_ix(FCE, exparr.len)

	if chunk >= len(exparr.chunks) {
    // Grow chunk array (doubling the size each time). With a FCE parameter of
    // 3, this means we must reallocate after growing past:
    // - 8 elements
    // - 24 elements
    // - 120 elements
    // - 2040 elements
    // - ...
    // As you can see, this grows very quickly (exponentially exponential).
    // There is an argument that we should start with enough space for 2
    // pointers right away, but I don't think it really matters right now. If I
    // ever feel like tuning this then I might add an additional type parameter
    // to the typed Exparr struct (although that would induce a lot of pain, so
    // I might also just not).
    data, length := mem.slice_to_components(exparr.chunks)
    new_length := max(1, 2 * length)

    layout := mem__layout(rawptr)
    raw := mem__resize(
      data,
      layout__array(layout, length),
      layout__array(layout, new_length),
      exparr.allocator,
    )

		exparr.chunks = mem.slice_ptr(cast(^rawptr)raw, new_length)
  }

  // Commit a new multipointer to the chunk array
  if exparr.chunks[chunk] == nil {
    exparr.chunks[chunk] = mem__alloc(
      layout__array(layout, 1 << (FCE + chunk)),
      exparr.allocator,
    )
  }

	ptr := mem__index(exparr.chunks[chunk], layout, lix)
	exparr.len += 1
	return ptr
}

exparr__push :: proc(
  exparr: ^Exparr($V, $FCE), element: V, loc := #caller_location
) -> ^V {
  ptr := cast(^V)exparr__repr__push(
    &exparr.repr,
    FCE,
    mem__layout(V),
    loc=loc
  )

	ptr^ = element
	return ptr
}
// }}}
// {{{ Pop
exparr__pop :: proc(x: ^Exparr($V, $FCE), loc := #caller_location) -> V {
  v := exparr__last(x^, loc = loc)^
  x.len -= 1
  return v
}
// }}}
// {{{ Reverse
exparr__repr__reverse :: proc(
  x: Exparr__Repr,
  FCE: uint,
  layout: Layout,
  temp: rawptr, // A temporary variable used for swapping things
) {
  for i in 0 ..< x.len / 2 {
    a := exparr__repr__get(x, FCE, layout, i)
    b := exparr__repr__get(x, FCE, layout, x.len - 1 - i)
    mem.copy(temp, a,    int(layout.size))
    mem.copy(a,    b,    int(layout.size))
    mem.copy(b,    temp, int(layout.size))
  }
}

exparr__reverse :: proc(x: Exparr($V, $FCE)) {
  temp: V
  exparr__repr__reverse(x, FCE, mem__layout(V), &temp)
}
// }}}

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
