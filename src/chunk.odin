package zen

import "core:fmt"
import "core:os"

/*
Each operation in bytecode has a one-byte operation code or opcode. These
control the instruction we're dealing with.

Note that the operation code is a byte, but the operation itself may consist of 
more than just one byte.
*/
OpCode :: enum {
	OP_NOOP,
	OP_CONSTANT,
	OP_NIL,
	OP_TRUE,
	OP_FALSE,
	OP_POP,
	OP_DUP,
	OP_GET_LOCAL,
	OP_SET_LOCAL,
	OP_GET_GLOBAL,
	OP_GET_UPVALUE,
	OP_SET_UPVALUE,
	OP_GET_PROPERTY,
	OP_SET_PROPERTY,
	OP_GET_SUPER,
	OP_GET_IT,
	OP_SET_IT,
	OP_DEFINE_GLOBAL,
	OP_SET_GLOBAL,
	OP_EQUAL,
	OP_GREATER,
	OP_LESS,
	OP_ADD,
	OP_SUBTRACT,
	OP_MULTIPLY,
	OP_DIVIDE,
	OP_MODULO,
	OP_NOT,
	OP_NEGATE,
	OP_PRINT,
	OP_JUMP,
	OP_JUMP_IF_FALSE,
	OP_JUMP_IF_TRUE,
	OP_LOOP,
	OP_CALL,
	OP_INVOKE,
	OP_SUPER_INVOKE,
	OP_LIST,
	OP_SUBSCRIPT,
	OP_SUBSCRIPT_SET,
	OP_CLOSURE,
	OP_CLOSE_UPVALUE,
	OP_RETURN,
	OP_CLASS,
	OP_INHERIT,
	OP_METHOD,
	OP_MODULE_BUILTIN,
	OP_MODULE_USER,
	OP_TOP_LEVEL_RETURN,
}

/*
A wrapper around a sequence of bytecode, stored as a dynamic array of bytes.
Additionally, it includes a constant pool and line number information.
*/
Chunk :: struct {
	// An array of bytes.
	code:      [dynamic]byte,

	// The constant pool.
	constants: ValueArray,

	// The line number. Encoded with RLE.
	lines:     [dynamic]int,
}

/*
Writes a line to an array using RLE.
*/
@(private = "file")
write_line :: proc(lines: ^[dynamic]int, line: int) {
	if len(lines^) == 0 {
		append(lines, 1)
		append(lines, line)
		return
	}

	if (lines^)[len(lines^) - 1] == line {
		(lines^)[len(lines^) - 2] += 1
	} else {
		append(lines, 1)
		append(lines, line)
	}
}

/*
Returns the line number for the instruction at the given offset.
*/
get_line :: proc(lines: [dynamic]int, offset: int) -> int {
	j := 0
	for line, idx in lines {
		if idx % 2 != 0 do continue

		for _ in 0 ..< line {
			if j == offset {
				return lines[idx + 1]
			}
			j += 1
		}
	}

	color_red(os.stderr, "bug: ")
	fmt.eprintf("Failed to find line number for offset %d\n", offset)
	unreachable()
}

/* Initializes a chunk. */
init_chunk :: proc() -> Chunk {
	return (Chunk {
				code = make([dynamic]byte, 0, 0),
				constants = init_value_array(),
				lines = make([dynamic]int, 0, 0),
			})
}

/* Writes a byte to a chunk. */
write_chunk :: proc(c: ^Chunk, byte: byte, line: int) {
	append(&c.code, byte)
	write_line(&c.lines, line)
}

/* Adds a new constant value to the chunk. */
add_constant :: proc(c: ^Chunk, gc: ^GC, value: Value) -> int {
	/* Temporarily push the value to the stack so that the GC can see it
	and doesn't sweep it away. */
	temp_push(gc, value)
	write_value_array(&c.constants, value)
	temp_pop(gc)
	return len(c.constants.values) - 1
}

/* Frees a chunk's memory. */
free_chunk :: proc(c: ^Chunk) {
	delete(c.code)
	delete(c.lines)
	free_value_array(&c.constants)
}
