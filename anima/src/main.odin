package anima

import "core:fmt"
main :: proc() {
	exparr: Exparr(uint, 3)
	exparr.allocator = context.temp_allocator

	for i in 0 ..< 1024 {
		exparr_push(&exparr, uint(i))
	}

	exparr_pop(&exparr)
	exparr_pop(&exparr)
	exparr_pop(&exparr)

	sum: uint = 0
	for i in 0 ..< exparr.len {
		sum += exparr_get(&exparr, i)^
	}

	fmt.println(sum)
	fmt.println(len(exparr.chunks))
}
