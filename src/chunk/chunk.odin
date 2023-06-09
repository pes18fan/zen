package chunk

import "core:fmt"

import v "../value"

/*
Each operation in bytecode has a one-byte operation code or opcode. These
control the instruction we're dealing with.
*/
OpCode :: enum {
    OP_CONSTANT,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NEGATE,
    OP_RETURN,
}

/*
A wrapper around a sequence of bytecode, stored as a dynamic array of bytes.
Additionally, it includes a constant pool and line number information.
*/
Chunk :: struct {
    // An array of bytes.
    code: [dynamic]byte,

    // The constant pool.
    constants: v.ValueArray,

    // The line number. Encoded with RLE.
    lines: [dynamic]int,
}

/*
Writes a line to an array using RLE.
! Memory leak here at line 66, according to the leak tracker in the main function,
apparently occuring every time a line is written. No idea why this is happening, 
if anyone does please let me know.
*/
@(private)
write_line :: proc (lines: ^[dynamic]int, line: int) {
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
get_line :: proc (lines: [dynamic]int, offset: int) -> int {
    j := 0
    for line, idx in lines {
        if idx % 2 != 0 do continue

        for _ in 0..<line {
            if j == offset {
                return lines[idx + 1]
            }
            j += 1
        }
    }

    fmt.eprintln("Invalid line number offset")
    unreachable()
}

/* Initializes a chunk. */
init_chunk :: proc () -> Chunk {
    return Chunk {
        code = make([dynamic]byte, 0, 0),
        constants = v.init_value_array(),
        lines = make([dynamic]int, 0, 0),
    }
}

/* Writes a byte to a chunk. */
write_chunk :: proc (c: ^Chunk, byte: byte, line: int) {
    append(&c.code, byte)
    write_line(&c.lines, line)
}

/* Adds a new constant value to the chunk. */
add_constant :: proc (c: ^Chunk, value: v.Value) -> int {
    v.write_value_array(&c.constants, value)
    return len(c.constants.values) - 1
}

/* Frees a chunk's memory. */
free_chunk :: proc (c: ^Chunk) {
    delete(c.code)
    delete(c.lines)
    v.free_value_array(&c.constants)
}
