package zen

import "core:fmt"

@(private="file")
simple_instruction :: proc (name: string, offset: int) -> int {
    fmt.eprintf("%s\n", name)
    return offset + 1
}

@(private="file")
byte_instruction :: proc (name: string, c: ^Chunk, offset: int) -> int {
    slot := c.code[offset + 1]
    fmt.eprintf("%-16s %4d\n", name, slot)
    return offset + 2
}

@(private="file")
jump_instruction :: proc (name: string, sign: int, c: ^Chunk, 
        offset: int) -> int {
    jump := int(c.code[offset + 1]) << 8
    jump |= int(c.code[offset + 2])
    fmt.eprintf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump)
    return offset + 3
}

@(private="file")
constant_instruction :: proc (name: string, c: ^Chunk, offset: int) -> int {
    constant := c.code[offset + 1]
    fmt.eprintf("%-16s %4d '", name, constant)
    print_value(c.constants.values[constant])
    fmt.eprintf("'\n")
    return offset + 2
}

/* Disassembles the instruction at the provided offset. */
disassemble_instruction :: proc (c: ^Chunk, offset: int) -> int {
    fmt.eprintf("%04d ", offset)

    if offset > 0 && 
        get_line(c.lines, offset) == get_line(c.lines, offset - 1) {
        fmt.eprintf("   | ")
    } else {
        fmt.eprintf("%4d ", get_line(c.lines, offset))
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
        case .OP_POP:
            return simple_instruction("OP_POP", offset)
        case .OP_GET_LOCAL:
            return byte_instruction("OP_GET_LOCAL", c, offset)
        case .OP_SET_LOCAL:
            return byte_instruction("OP_SET_LOCAL", c, offset)
        case .OP_GET_GLOBAL:
            return constant_instruction("OP_GET_GLOBAL", c, offset)
        case .OP_DEFINE_GLOBAL:
            return constant_instruction("OP_DEFINE_GLOBAL", c, offset)
        case .OP_EQUAL:
            return simple_instruction("OP_EQUAL", offset)
        case .OP_SET_GLOBAL:
            return constant_instruction("OP_SET_GLOBAL", c, offset)
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
        case .OP_PRINT:
            return simple_instruction("OP_PRINT", offset)
        case .OP_JUMP:
            return jump_instruction("OP_JUMP", 1, c, offset)
        case .OP_JUMP_IF_FALSE:
            return jump_instruction("OP_JUMP_IF_FALSE", 1, c, offset)
        case .OP_LOOP:
            return jump_instruction("OP_LOOP", -1, c, offset) 
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
    fmt.eprintf("== %s ==\n", name)

    for i := 0; i < len(c.code); {
        i = disassemble_instruction(c, i)
    }
}

