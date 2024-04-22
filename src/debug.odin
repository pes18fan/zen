package zen

import "core:fmt"

/* Print debug info for a simple instruction. */
@(private = "file")
simple_instruction :: proc(name: string, offset: int) -> int {
	fmt.eprintf("%s\n", name)
	return offset + 1
}

/* Print debug info for an instruction with one byte operand. */
@(private = "file")
byte_instruction :: proc(name: string, c: ^Chunk, offset: int) -> int {
	slot := c.code[offset + 1]
	fmt.eprintf("%-16s %4d\n", name, slot)
	return offset + 2
}

/* Print debug info for a jump instruction. */
@(private = "file")
jump_instruction :: proc(name: string, sign: int, c: ^Chunk, offset: int) -> int {
	jump := int(c.code[offset + 1]) << 8
	jump |= int(c.code[offset + 2])
	fmt.eprintf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump)
	return offset + 3
}

/* Print debug info for an instruction that loads a constant. */
@(private = "file")
constant_instruction :: proc(name: string, c: ^Chunk, offset: int) -> int {
	constant := c.code[offset + 1]
	fmt.eprintf("%-16s %4d '", name, constant)
	str, was_allocation := stringify_value(c.constants.values[constant])
	defer if was_allocation {delete(str)}
	fmt.eprintf(str)
	fmt.eprintf("'\n")
	return offset + 2
}

@(private = "file")
class_instruction :: proc(name: string, c: ^Chunk, offset: int) -> int {
	constant := c.code[offset + 1]
	public := bool(c.code[offset + 1])
	fmt.eprintf("%-16s %4d '", name, constant)

	str, was_allocation := stringify_value(c.constants.values[constant])
	defer if was_allocation {delete(str)}

	fmt.eprintf(str)
	fmt.eprintf("'")
	fmt.eprintf(", %s", public ? "public" : "private")
	fmt.eprintf("\n")
	return offset + 3
}

/* Print debug info for a user-defined module import. */
@(private = "file")
user_module_instruction :: proc(name: string, c: ^Chunk, offset: int) -> int {
	module_name, mn_was_allocation := stringify_value(c.constants.values[c.code[offset + 1]])
	module_path, mp_was_allocation := stringify_value(c.constants.values[c.code[offset + 2]])

	defer if mn_was_allocation {delete(module_name)}
	defer if mp_was_allocation {delete(module_path)}

	fmt.eprintf("%-16s (module '%s', path '%s')\n", name, module_name, module_path)
	return offset + 3
}

/* Print debug info for the OP_INVOKE opcode. */
@(private = "file")
invoke_instruction :: proc(name: string, c: ^Chunk, offset: int) -> int {
	constant := c.code[offset + 1]
	arg_count := c.code[offset + 2]
	fmt.eprintf("%-16s (%d args) %4d '", name, arg_count, constant)

	str, was_allocation := stringify_value(c.constants.values[constant])
	defer if was_allocation {delete(str)}
	fmt.eprintf(str)
	fmt.eprintf("'\n")
	return offset + 3
}

/* Disassembles the instruction at the provided offset. */
disassemble_instruction :: proc(c: ^Chunk, offset: int) -> int {
	fmt.eprintf("%04d ", offset)

	if offset > 0 && get_line(c.lines, offset) == get_line(c.lines, offset - 1) {
		fmt.eprintf("   | ")
	} else {
		fmt.eprintf("   %d ", get_line(c.lines, offset))
	}

	instruction := c.code[offset]
	switch OpCode(instruction) {
	case .OP_NOOP:
		return simple_instruction("OP_NOOP", offset)
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
	case .OP_DUP:
		return simple_instruction("OP_DUP", offset)
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
	case .OP_GET_UPVALUE:
		return byte_instruction("OP_GET_UPVALUE", c, offset)
	case .OP_SET_UPVALUE:
		return byte_instruction("OP_SET_UPVALUE", c, offset)
	case .OP_GET_PROPERTY:
		return constant_instruction("OP_GET_PROPERTY", c, offset)
	case .OP_SET_PROPERTY:
		return constant_instruction("OP_SET_PROPERTY", c, offset)
	case .OP_GET_SUPER:
		return constant_instruction("OP_GET_SUPER", c, offset)
	case .OP_GET_IT:
		return simple_instruction("OP_GET_IT", offset)
	case .OP_SET_IT:
		return simple_instruction("OP_SET_IT", offset)
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
	case .OP_JUMP_IF_TRUE:
		return jump_instruction("OP_JUMP_IF_TRUE", 1, c, offset)
	case .OP_LOOP:
		return jump_instruction("OP_LOOP", -1, c, offset)
	case .OP_CALL:
		return byte_instruction("OP_CALL", c, offset)
	case .OP_INVOKE:
		return invoke_instruction("OP_INVOKE", c, offset)
	case .OP_SUPER_INVOKE:
		return invoke_instruction("OP_SUPER_INVOKE", c, offset)
	case .OP_LIST:
		return byte_instruction("OP_LIST", c, offset)
	case .OP_SUBSCRIPT:
		return simple_instruction("OP_SUBSCRIPT", offset)
	case .OP_CLOSURE:
		{
			offset := offset
			offset += 1
			constant := c.code[offset]
			fmt.eprintf("%-16s %4d ", "OP_CLOSURE", constant)
			str, was_allocation := stringify_value(c.constants.values[constant])
			defer if was_allocation {
				delete(str)
			}
			fmt.eprintf(str)
			offset += 1
			public := bool(c.code[offset])
			fmt.eprintf(", %s", public ? "public" : "private")
			fmt.eprintln()
			offset += 1

			function := as_function(c.constants.values[constant])
			for j in 0 ..< function.upvalue_count {
				is_local := bool(c.code[offset])
				offset += 1
				index := c.code[offset]
				offset += 1

				fmt.eprintf(
					"%04d	  |                     %s %d\n",
					offset - 2,
					is_local ? "local" : "upvalue",
					index,
				)
			}

			return offset
		}
	case .OP_CLOSE_UPVALUE:
		return simple_instruction("OP_CLOSE_UPVALUE", offset)
	case .OP_RETURN:
		return simple_instruction("OP_RETURN", offset)
	case .OP_CLASS:
		return class_instruction("OP_CLASS", c, offset)
	case .OP_INHERIT:
		return simple_instruction("OP_INHERIT", offset)
	case .OP_METHOD:
		return constant_instruction("OP_METHOD", c, offset)
	case .OP_MODULE_BUILTIN:
		return constant_instruction("OP_MODULE_BUILTIN", c, offset)
	case .OP_MODULE_USER:
		return user_module_instruction("OP_MODULE_USER", c, offset)
	case:
		fmt.eprintf("Unknown opcode %d\n", instruction)
		return offset + 1
	}
}

/*
Prints a header for each chunk, then loops through the bytecode, 
disassembling each instruction one by one.
*/
disassemble :: proc(c: ^Chunk, name: string) {
	fmt.eprintf("== %s ==\n", name)

	for i := 0; i < len(c.code); {
		i = disassemble_instruction(c, i)
	}
}
