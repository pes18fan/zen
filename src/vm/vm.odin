package vm

import "core:fmt"

import ch "../chunk"
import cmp "../compiler"
import dbg "../debug"
import lx "../lexer"
import val "../value"

/* The maximum size for the stack. Going past this causes a stack overflow. */
STACK_MAX :: 256

/* The virtual machine that interprets the bytecode. */
VM :: struct {
    /* The chunk being interpreted. */
    chunk: ^ch.Chunk,

    /* Instruction pointer, although its an index. Represents where the VM
    is in the bytecod array. */
    ip: int,

    /* The stack of values. */
    stack: [dynamic]val.Value,
}

/* The result of the interpreting. */
InterpretResult :: enum {
    INTERPRET_OK,
    INTERPRET_LEX_ERROR,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
}

/* Returns a newly created VM. */
init_VM :: proc () -> VM {
    return VM {
        chunk = nil,
        ip = 0,
        stack = make([dynamic]val.Value, 0, 0),
    }
}

/* Free's the VM's memory. */
free_VM :: proc (v: ^VM) {
    delete(v.stack)
}

/* Reads a byte from the chunk and increments the instruction pointer. */
@(private)
read_byte :: proc (v: ^VM) -> byte {
    v.ip = v.ip + 1
    return v.chunk.code[v.ip - 1]
}

/* Reads a constant from the chunk and pushes it onto the stack. */
@(private)
read_constant :: proc (v: ^VM) -> val.Value {
    constant := v.chunk.constants.values[read_byte(v)]
    return constant
}

/* Performs a binary operation on the top two values of the stack. */
@(private)
binary_op :: proc (v: ^VM, op: byte) {
    b := vm_pop(v)
    a := vm_pop(v)

    switch op {
        case '+': vm_push(v, a + b)
        case '-': vm_push(v, a - b)
        case '*': vm_push(v, a * b)
        case '/': vm_push(v, a / b)
    }
}

/*
Run the VM, going through the bytecode and interpreting each instruction
one by one.
*/
@(private)
run :: proc (v: ^VM) -> InterpretResult {
    using ch.OpCode

    for {
        when ODIN_DEBUG {
            fmt.printf("          ")
            for i in v.stack {
                fmt.printf("[ ")
                val.print_value(i)
                fmt.printf(" ]")
            }
            fmt.printf("\n")

            dbg.disassemble_instruction(v.chunk, v.ip)
        }

        instruction := ch.OpCode(read_byte(v))

        switch instruction {
            case .OP_CONSTANT:
                constant := read_constant(v)
                vm_push(v, constant)
                val.print_value(constant)
                fmt.printf("\n")
            case .OP_ADD:      binary_op(v, '+')
            case .OP_SUBTRACT: binary_op(v, '-')
            case .OP_MULTIPLY: binary_op(v, '*')
            case .OP_DIVIDE:   binary_op(v, '/')
            case .OP_NEGATE:   vm_push(v, -vm_pop(v))
            case .OP_RETURN: 
                val.print_value(vm_pop(v))
                fmt.printf("\n")
                return .INTERPRET_OK
        }
    }
}

/* Interpret a chunk. */
interpret :: proc (v: ^VM, source: string) -> InterpretResult {
    lexer := lx.init_lexer(source)
    tokens, err := lx.lex(&lexer)
    defer delete(tokens)
    if err != nil {
        return .INTERPRET_LEX_ERROR
    }

    chunk := ch.init_chunk()
    defer ch.free_chunk(&chunk)
    cmp_ok := cmp.compile(tokens, &chunk)
    if !cmp_ok {
        return .INTERPRET_COMPILE_ERROR
    }

    v.chunk = &chunk
    v.ip = 0

    result := run(v)

    return result
}

/* Push a value onto the stack. */
vm_push :: proc (v: ^VM, value: val.Value) {
    append(&v.stack, value)
}

/* Pop a value out of the stack. */
vm_pop :: proc (v: ^VM) -> val.Value {
    return pop(&v.stack)
}
