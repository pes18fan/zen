package zen

import "core:fmt"

@(private="file")
simple_instruction :: proc (name: string, offset: int) -> int {
    fmt.printf("%s\n", name)
    return offset + 1
}

@(private="file")
constant_instruction :: proc (name: string, c: ^Chunk, offset: int) -> int {
    constant := c.code[offset + 1]
    fmt.printf("%-16s %4d '", name, constant)
    print_value(c.constants.values[constant])
    fmt.printf("'\n")
    return offset + 2
}

/* Disassembles the instruction at the provided offset. */
disassemble_instruction :: proc (c: ^Chunk, offset: int) -> int {
    fmt.printf("%04d ", offset)

    if offset > 0 && 
        get_line(c.lines, offset) == get_line(c.lines, offset - 1) {
        fmt.printf("   | ")
    } else {
        fmt.printf("%4d ", get_line(c.lines, offset))
    }

    instruction := c.code[offset]
    switch OpCode(instruction) {
        case .OP_CONSTANT:
            return constant_instruction("OP_CONSTANT", c, offset)
        case .OP_NIL:
            return simple_instruction("OP_NIL", offset)
        case .OP_TRUE:
            return simple_instruction("OP_TRUE", offset)
        case .OP_FALSE:
            return simple_instruction("OP_FALSE", offset)
        case .OP_EQUAL:
            return simple_instruction("OP_EQUAL", offset)
        case .OP_GREATER:
            return simple_instruction("OP_GREATER", offset)
        case .OP_LESS:
            return simple_instruction("OP_LESS", offset)
        case .OP_ADD:
            return simple_instruction("OP_ADD", offset)
        case .OP_SUBTRACT:
            return simple_instruction("OP_SUBTRACT", offset)
        case .OP_MULTIPLY:
            return simple_instruction("OP_MULTIPLY", offset)
        case .OP_DIVIDE:
            return simple_instruction("OP_DIVIDE", offset)
        case .OP_NOT:
            return simple_instruction("OP_NOT", offset)
        case .OP_NEGATE:
            return simple_instruction("OP_NEGATE", offset)
        case .OP_RETURN:
            return simple_instruction("OP_RETURN", offset)
        case:
            fmt.eprintf("Unknown opcode %d\n", instruction)
            return offset + 1
    }
}

/*
Prints a header for each chunk, then loops through the bytecode, 
disassembling each instruction one by one.
*/
disassemble :: proc (c: ^Chunk, name: string) {
    fmt.printf("== %s ==\n", name)

    for i := 0; i < len(c.code); {
        i = disassemble_instruction(c, i)
    }
}

