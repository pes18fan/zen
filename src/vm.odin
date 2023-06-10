package zen

import "core:fmt"
import "core:mem"
import "core:strings"

/* The maximum size for the stack. Going past this causes a stack overflow. */
STACK_MAX :: 256

/* The virtual machine that interprets the bytecode. */
VM :: struct {
    /* The chunk being interpreted. */
    chunk: ^Chunk,

    /* Instruction pointer, although its an index. Represents where the VM
    is in the bytecod array. */
    ip: int,

    /* The stack of values. */
    stack: [dynamic]Value,

    globals: Table,
    strings: Table,

    objects: ^Obj,
}

/* The result of the interpreting. */
InterpretResult :: enum {
    INTERPRET_OK,
    INTERPRET_LEX_ERROR,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
}

/* Raise a runtime error. */
runtime_error :: proc (vm: ^VM, format: string, args: ..any) {
    fmt.eprintf(format, ..args)
    fmt.eprintln()

    line := get_line(vm.chunk.lines, vm.ip - 1)
    fmt.eprintf("[line %d] in script\n", line)
    reset_stack(vm)
}

reset_stack :: proc (vm: ^VM) {
    defer {
        delete(vm.stack)
        vm.stack = make([dynamic]Value, 0, 0)
    }
    vm.chunk = nil
    vm.ip = 0
}

/* Returns a newly created VM. */
init_VM :: proc () -> VM {
    return VM {
        chunk = nil,
        ip = 0,
        stack = make([dynamic]Value, 0, 0),
        objects = nil,
        globals = init_table(),
        strings = init_table(),
    }
}

/* Free's the VM's memory. */
free_VM :: proc (vm: ^VM) {
    free_table(&vm.globals)
    free_table(&vm.strings)
    free_objects(vm)
    delete(vm.stack)
}

/* Reads a byte from the chunk and increments the instruction pointer. */
@(private="file")
read_byte :: proc (vm: ^VM) -> byte {
    vm.ip += 1
    return vm.chunk.code[vm.ip - 1]
}

/* Reads a constant from the chunk and pushes it onto the stack. */
@(private="file")
read_constant :: proc (vm: ^VM) -> Value {
    return vm.chunk.constants.values[read_byte(vm)]
}

@(private="file")
read_string :: proc (vm: ^VM) -> ^ObjString {
    return as_string(read_constant(vm))
}

/*
Performs a binary operation on the top two values of the stack. In zen, a
binary operator can only return either a 64-bit float or a boolean. 
*/
@(private="file")
binary_op :: proc (v: ^VM, $Returns: typeid, op: byte) -> bool {
    if !is_number(vm_peek(v, 0)) || !is_number(vm_peek(v, 1)) {
        runtime_error(v, "Operands must be numbers.")
        return false
    }

    b := as_number(vm_pop(v))
    a := as_number(vm_pop(v))

    switch typeid_of(Returns) {
        case f64:
            switch op {
                case '+': vm_push(v, number_val(a + b))
                case '-': vm_push(v, number_val(a - b))
                case '*': vm_push(v, number_val(a * b))
                case '/': vm_push(v, number_val(a / b))
            }
        case bool:
            switch op {
                case '>': vm_push(v, bool_val(a > b))
                case '<': vm_push(v, bool_val(a < b))
            }
        case: unreachable()
    }

    return true
}

/*
Run the VM, going through the bytecode and interpreting each instruction
one by one.
*/
@(private="file")
run :: proc (v: ^VM) -> InterpretResult {
    using OpCode

    for {
        when ODIN_DEBUG {
            fmt.printf("          ")
            for i in v.stack {
                fmt.printf("[ ")
                print_value(i)
                fmt.printf(" ]")
            }
            fmt.printf("\n")

            disassemble_instruction(v.chunk, v.ip)
        }

        instruction := OpCode(read_byte(v))

        switch instruction {
            case .OP_CONSTANT:
                constant := read_constant(v)
                vm_push(v, constant)
                // print_value(constant)
                // fmt.printf("\n")
            case .OP_NIL:      vm_push(v, nil_val())
            case .OP_TRUE:     vm_push(v, bool_val(true))
            case .OP_FALSE:    vm_push(v, bool_val(false))
            case .OP_POP:      vm_pop(v)
            case .OP_GET_GLOBAL: {
                name := read_string(v)
                value: Value; ok: bool
                if value, ok = table_get(&v.globals, name); !ok {
                    runtime_error(v, "Undefined variable '%s'.", name.chars)
                    return .INTERPRET_RUNTIME_ERROR
                }
                vm_push(v, value)
            }
            case .OP_DEFINE_GLOBAL:
                name := read_string(v)
                table_set(&v.globals, name, vm_peek(v, 0))
                vm_pop(v)
            case .OP_SET_GLOBAL: {
                name := read_string(v)

                if table_set(&v.globals, name, vm_peek(v, 0)) {
                    table_delete(&v.globals, name)
                    runtime_error(v, "Undefined variable '%s'.", name.chars)
                    return .INTERPRET_RUNTIME_ERROR
                }
            }
            case .OP_EQUAL: {
                b := vm_pop(v)
                a := vm_pop(v)
                vm_push(v, bool_val(values_equal(a, b)))
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
                if is_string(vm_peek(v, 0)) && 
                    is_string(vm_peek(v, 1)) {
                    concatenate(v)
                } else if is_number(vm_peek(v, 0)) && 
                    is_number(vm_peek(v, 1)) {
                    b := as_number(vm_pop(v))
                    a := as_number(vm_pop(v))
                    vm_push(v, number_val(a + b))
                } else {
                    runtime_error(v, 
                        "Operands must be two numbers or two strings.")
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
                vm_push(v, bool_val(is_falsey(vm_pop(v))))
            case .OP_NEGATE:
                if !is_number(vm_peek(v, 0)) {
                    runtime_error(v, "Can only negate numbers.")
                    return .INTERPRET_RUNTIME_ERROR
                }
                vm_push(v, number_val(-as_number(vm_pop(v))))
            case .OP_WRITE:
                print_value(vm_pop(v))
                fmt.printf("\n")
            case .OP_RETURN: 
                // Exit interpreter.
                return .INTERPRET_OK
        }
    }
}

/* Interpret a chunk. */
interpret :: proc (vm: ^VM, source: string) -> InterpretResult {
    lexer := init_lexer(source)
    tokens, err := lex(&lexer)
    defer delete(tokens)
    if err != nil {
        return .INTERPRET_LEX_ERROR
    }

    chunk := init_chunk()
    defer free_chunk(&chunk)
    cmp_ok := compile(vm, tokens, &chunk)
    if !cmp_ok {
        return .INTERPRET_COMPILE_ERROR
    }

    vm.chunk = &chunk
    vm.ip = 0

    result := run(vm)

    return result
}

/* Push a value onto the stack. */
vm_push :: proc (vm: ^VM, value: Value) {
    append(&vm.stack, value)
}

/* Pop a value out of the stack. */
vm_pop :: proc (vm: ^VM) -> Value {
    return pop(&vm.stack)
}

/* Peek at a certain distance from the top of the stack. */
vm_peek :: proc (vm: ^VM, distance: int) -> Value {
    return vm.stack[len(vm.stack) - 1 - distance]
}

/* Returns true if provided value is falsey. */
is_falsey :: proc (value: Value) -> bool {
    return is_nil(value) || (is_bool(value) && !as_bool(value))
}

/* Concatenate two strings. */
concatenate :: proc (vm: ^VM) {
    b := as_string(vm_pop(vm))
    a := as_string(vm_pop(vm))

    length := len(a.chars) + len(b.chars)
    chars := make([]byte, length)
    i := 0
    i =+ copy(chars, a.chars)
    copy(chars[i:], b.chars)

    result := take_string(vm, string(chars))
    vm_push(vm, (^Obj)(result))
}

free_objects :: proc (vm: ^VM) {
    object := vm.objects

    for object != nil {
        next := object.next
        free_object(object)
        object = next
    }
}
