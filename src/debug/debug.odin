package debug

import "core:fmt"
import ch "../chunk"
import v "../value"

@(private)
simple_instruction :: proc (name: string, offset: int) -> int {
    fmt.printf("%s\n", name)
    return offset + 1
}

@(private)
constant_instruction :: proc (name: string, c: ^ch.Chunk, offset: int) -> int {
    constant := c.code[offset + 1]
    fmt.printf("%-16s %4d '", name, constant)
    v.print_value(c.constants.values[constant])
    fmt.printf("'\n")
    return offset + 2
}

/* Disassembles the instruction at the provided offset. */
disassemble_instruction :: proc (c: ^ch.Chunk, offset: int) -> int {
    using ch.OpCode

    fmt.printf("%04d ", offset)

    if offset > 0 && 
        ch.get_line(c.lines, offset) == ch.get_line(c.lines, offset - 1) {
        fmt.printf("   | ")
    } else {
        fmt.printf("%4d ", ch.get_line(c.lines, offset))
    }

    instruction := c.code[offset]
    switch ch.OpCode(instruction) {
        case OP_CONSTANT:
            return constant_instruction("OP_CONSTANT", c, offset)
        case OP_ADD:
            return simple_instruction("OP_ADD", offset)
        case OP_SUBTRACT:
            return simple_instruction("OP_SUBTRACT", offset)
        case OP_MULTIPLY:
            return simple_instruction("OP_MULTIPLY", offset)
        case OP_DIVIDE:
            return simple_instruction("OP_DIVIDE", offset)
        case OP_NEGATE:
            return simple_instruction("OP_NEGATE", offset)
        case OP_RETURN:
            return simple_instruction("OP_RETURN", offset)
        case:
            fmt.printf("Unknown opcode %d\n", instruction)
            return offset + 1
    }
}

/*
Prints a header for each chunk, then loops through the bytecode, 
disassembling each instruction one by one.
*/
disassemble :: proc (c: ^ch.Chunk, name: string) {
    fmt.printf("== %s ==\n", name)

    for i := 0; i < len(c.code); {
        i = disassemble_instruction(c, i)
    }
}

