package zen

import "core:slice"
import tt "core:testing"

/*
Write a random chunk with arbitrary line numbers and check if those line
numbers are RLE encoded. Fail if that's not the case.
*/
@(test)
test_chunk_write_lines :: proc(t: ^tt.T) {
	chunk := init_chunk()
	defer free_chunk(&chunk)

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

	tt.expectf(
		t,
		len(got) == len(want),
		"encode_lines(%v) = \ngot %v, \nwant %v",
		chunk.lines,
		got,
		want,
	)
	tt.expectf(
		t,
		slice.equal(got, want),
		"encode_lines(%v) = \ngot %v, \nwant %v",
		chunk.lines,
		got,
		want,
	)
}

/* Test if the RLE encoded line numbers are properly decoded. */
@(test)
test_chunk_get_line :: proc(t: ^tt.T) {
	lines := make([dynamic]int)
	defer delete(lines)

	append(&lines, 3, 1, 2, 2, 3, 3, 2, 4)

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

/* Test that an empty chunk has no lines and no instructions. */
@(test)
test_chunk_empty :: proc(t: ^tt.T) {
	chunk := init_chunk()
	defer free_chunk(&chunk)

	tt.expectf(t, len(chunk.code) == 0, "expected empty chunk, got %v instructions", len(chunk.code))
	tt.expectf(t, len(chunk.lines) == 0, "expected empty lines, got %v", len(chunk.lines))
}

/* Test writing and counting a single instruction. */
@(test)
test_chunk_single_instruction :: proc(t: ^tt.T) {
	chunk := init_chunk()
	defer free_chunk(&chunk)

	write_chunk(&chunk, byte(OpCode.OP_RETURN), 42)
	tt.expect_value(t, len(chunk.code), 1)
	tt.expect_value(t, chunk.code[0], byte(OpCode.OP_RETURN))
	tt.expect_value(t, get_line(chunk.lines, 0), 42)
}

/* Test that writing on the same line appends to the RLE run. */
@(test)
test_chunk_same_line_encoding :: proc(t: ^tt.T) {
	chunk := init_chunk()
	defer free_chunk(&chunk)

	write_chunk(&chunk, byte(OpCode.OP_RETURN), 10)
	write_chunk(&chunk, byte(OpCode.OP_NIL), 10)
	write_chunk(&chunk, byte(OpCode.OP_POP), 10)

	got := chunk.lines
	tt.expectf(t, len(got) == 2, "expected 2 RLE values, got %v", len(got))
	tt.expect_value(t, got[0], 3)
	tt.expect_value(t, got[1], 10)
}

/* Test that writing to consecutive different lines creates multiple runs. */
@(test)
test_chunk_diff_line_encoding :: proc(t: ^tt.T) {
	chunk := init_chunk()
	defer free_chunk(&chunk)

	write_chunk(&chunk, byte(OpCode.OP_RETURN), 1)
	write_chunk(&chunk, byte(OpCode.OP_RETURN), 2)
	write_chunk(&chunk, byte(OpCode.OP_RETURN), 3)

	got := chunk.lines
	tt.expectf(t, len(got) == 6, "expected 6 RLE values (3 runs), got %v", len(got))
	tt.expect_value(t, got[0], 1)  // run length
	tt.expect_value(t, got[1], 1)  // line number
	tt.expect_value(t, got[2], 1)  // run length
	tt.expect_value(t, got[3], 2)  // line number
	tt.expect_value(t, got[4], 1)  // run length
	tt.expect_value(t, got[5], 3)  // line number
}
