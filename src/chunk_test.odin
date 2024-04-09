package zen

import "core:fmt"
import "core:slice"
import tt "core:testing"

/*
Write a random chunk with arbitrary line numbers and check if those line
numbers are RLE encoded. Fail if that's not the case.
*/
@(test)
test_chunk_write_lines :: proc(t: ^tt.T) {
	chunk := init_chunk()
	/* Write a chunk with line numbers upto 20. If `i` is even, write an extra 
    instruction. The contents of the chunk don't matter here since this is a 
    test for the line number encoding. */
	for i in 1 ..= 20 {
		write_chunk(&chunk, byte(OpCode.OP_RETURN), i)

		if i % 2 == 0 {
			write_chunk(&chunk, byte(OpCode.OP_RETURN), i)
		}
	}
	got := chunk.lines[:]
	want := []int {
		1,
		1,
		2,
		2,
		1,
		3,
		2,
		4,
		1,
		5,
		2,
		6,
		1,
		7,
		2,
		8,
		1,
		9,
		2,
		10,
		1,
		11,
		2,
		12,
		1,
		13,
		2,
		14,
		1,
		15,
		2,
		16,
		1,
		17,
		2,
		18,
		1,
		19,
		2,
		20,
	}

	if len(got) != len(want) {
		tt.errorf(t, "encode_lines(%v) = \ngot %v, \nwant %v", chunk.lines, got, want)
	}

	if !slice.equal(got, want) {
		tt.errorf(t, "encode_lines(%v) = \ngot %v, \nwant %v", chunk.lines, got, want)
	}
}

/* Test if the RLE encoded line numbers are properly decoded. */
@(test)
test_chunk_get_line :: proc(t: ^tt.T) {
	lines := [dynamic]int{3, 1, 2, 2, 3, 3, 2, 4}

	tt.expect_value(t, get_line(lines, 0), 1)
	tt.expect_value(t, get_line(lines, 1), 1)
	tt.expect_value(t, get_line(lines, 2), 1)
	tt.expect_value(t, get_line(lines, 3), 2)
	tt.expect_value(t, get_line(lines, 4), 2)
	tt.expect_value(t, get_line(lines, 5), 3)
	tt.expect_value(t, get_line(lines, 6), 3)
	tt.expect_value(t, get_line(lines, 7), 3)
	tt.expect_value(t, get_line(lines, 8), 4)
	tt.expect_value(t, get_line(lines, 9), 4)
}
