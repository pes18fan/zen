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

/* Raise a runtime error. */
runtime_error :: proc (v: ^VM, format: string, args: ..any) {
    fmt.eprintf(format, ..args)
    fmt.eprintln()

    line := ch.get_line(v.chunk.lines, v.ip - 1)
    fmt.eprintf("[line %d] in script\n", line)
    reset_stack(v)
}

reset_stack :: proc (v: ^VM) {
    defer {
        delete(v.stack)
        v.stack = make([dynamic]val.Value, 0, 0)
    }
    v.chunk = nil
    v.ip = 0
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

/*
Performs a binary operation on the top two values of the stack. In zen, a
binary operator can only return either a 64-bit float or a boolean. 
*/
@(private)
binary_op :: proc (v: ^VM, $Returns: typeid, op: byte) -> bool {
    if !val.is_number(vm_peek(v, 0)) || !val.is_number(vm_peek(v, 1)) {
        runtime_error(v, "Operands must be numbers.")
        return false
    }

    b := val.as_number(vm_pop(v))
    a := val.as_number(vm_pop(v))

    switch typeid_of(Returns) {
        case f64:
            switch op {
                case '+': vm_push(v, val.number_val(a + b))
                case '-': vm_push(v, val.number_val(a - b))
                case '*': vm_push(v, val.number_val(a * b))
                case '/': vm_push(v, val.number_val(a / b))
            }
        case bool:
            switch op {
                case '>': vm_push(v, val.bool_val(a > b))
                case '<': vm_push(v, val.bool_val(a < b))
            }
        case: unreachable()
    }

    return true
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
            case .OP_NIL:      vm_push(v, val.nil_val())
            case .OP_TRUE:     vm_push(v, val.bool_val(true))
            case .OP_FALSE:    vm_push(v, val.bool_val(false))
            case .OP_EQUAL: {
                b := vm_pop(v)
                a := vm_pop(v)
                vm_push(v, val.bool_val(val.values_equal(a, b)))
            }
            // TODO: Get rid of the below repitition for binary operations
            case .OP_GREATER:
                ok := binary_op(v, bool, '>')
                if !ok {
                    return .INTERPRET_RUNTIME_ERROR
                }
            case .OP_LESS:
                ok := binary_op(v, bool, '<')
                if !ok {
                    return .INTERPRET_RUNTIME_ERROR
                }
            case .OP_ADD:      
                ok := binary_op(v, f64, '+')
                if !ok {
                    return .INTERPRET_RUNTIME_ERROR
                }
            case .OP_SUBTRACT: 
                ok := binary_op(v, f64, '-')
                if !ok {
                    return .INTERPRET_RUNTIME_ERROR
                }
            case .OP_MULTIPLY: 
                ok := binary_op(v, f64, '*')
                if !ok {
                    return .INTERPRET_RUNTIME_ERROR
                }
            case .OP_DIVIDE:   
                ok := binary_op(v, f64, '/')
                if !ok {
                    return .INTERPRET_RUNTIME_ERROR
                }
            case .OP_NOT:
                vm_push(v, val.bool_val(is_falsey(vm_pop(v))))
            case .OP_NEGATE:
                if !val.is_number(vm_peek(v, 0)) {
                    runtime_error(v, "Can only negate numbers.")
                    return .INTERPRET_RUNTIME_ERROR
                }
                vm_push(v, val.number_val(-val.as_number(vm_pop(v))))
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

/* Peek at a certain distance from the top of the stack. */
vm_peek :: proc (v: ^VM, distance: int) -> val.Value {
    return v.stack[len(v.stack) - 1 - distance]
}

/* Returns true if provided value is falsey. */
is_falsey :: proc (value: val.Value) -> bool {
    return val.is_nil(value) || (val.is_bool(value) && !val.as_bool(value))
}
