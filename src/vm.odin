package zen

import "core:fmt"
import "core:mem"
import "core:math"
import "core:strings"
import "core:time"

FRAMES_MAX :: 64

/* The maximum size for the stack. Going past this causes a stack overflow. */
STACK_MAX :: FRAMES_MAX * U8_COUNT

/*
A call frame.
Each function gets a small "window" or "frame" within the larger stack of the
VM itself, where it stores its own locals and whatnot.
A callframe represents a single ongoing function call. 
A callframe also must have a return address to go back to, but the struct
doesn't store it. The caller of a function represented by a callframe stores
its own `ip`, and when we return from the function, the VM jumps back to the
`ip` of the caller's callframe.
*/
CallFrame :: struct {
    function: ^ObjFunction,

    /* Pointer to the current instruction in the frame. */
    ip: ^byte,

    /* A slice of the VM's main stack. */
    slots: []Value,
}

/* The virtual machine that interprets the bytecode. */
VM :: struct {
    /* The chunk being interpreted. */
    chunk: ^Chunk,

    /* The stack of values. */
    stack: [dynamic]Value,

    /* Call frames present in the chunk. */
    frames: [FRAMES_MAX]CallFrame,
    frame_count: int,

    /* Table of runtime global variables. */
    globals: Table,

    /* Table of compile-time global variables; necessary for the REPL. */
    compiler_globals: Table,
    strings: Table,

    /* Linked list of all objects allocated by the VM. */
    objects: ^Obj,
}

/* NATIVE FUNCTIONS */
clock_native :: proc (arg_count: int, args: []Value) -> Value {
    return number_val(f64(time.to_unix_nanoseconds(time.now())) / 1e9)
}

/* The result of the interpreting. */
InterpretResult :: enum {
    INTERPRET_OK,
    INTERPRET_LEX_ERROR,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
}

/* Raise a runtime error. */
vm_panic :: proc (vm: ^VM, format: string, args: ..any) {
    fmt.eprint(COL_RED, "panic:", RESET)
    fmt.eprintf("%s", fmt.tprintf(format, ..args))
    fmt.eprintln()

    for i := vm.frame_count - 1; i >= 0; i -= 1 {
        frame := &vm.frames[i]
        function := frame.function
        instruction := mem.ptr_sub(frame.ip, &frame.function.chunk.code[0]) - 1
        line := get_line(function.chunk.lines, instruction)

        fmt.eprintf("  from [line %d] in", line)
        if function.name == nil {
            fmt.eprintf(" script\n")
        } else {
            fmt.eprintf(" %s()\n", function.name.chars)
        }
    }

    reset_stack(vm)
}

/* Define a native function. */
define_native :: proc (vm: ^VM, name: string, function: NativeFn) {
    vm_push(vm, obj_val(copy_string(vm, name)))
    vm_push(vm, obj_val(new_native(vm, function)))
    table_set(&vm.globals, as_string(vm.stack[0]), vm.stack[1])
    vm_pop(vm)
    vm_pop(vm)
}

/* Resets the stack. */
reset_stack :: proc (vm: ^VM) {
    defer {
        delete(vm.stack)
        vm.stack = make([dynamic]Value, 0, 0)
    }
    vm.chunk = nil
    vm.frame_count = 0
}

/* Returns a newly created VM. */
init_VM :: proc () -> VM {
    vm := VM {
        chunk = nil,
        stack = make([dynamic]Value, 0, 0),
        objects = nil,
        globals = init_table(),
        compiler_globals = init_table(),
        strings = init_table(),
        frame_count = 0,
    }

    define_native(&vm, "clock", clock_native)

    return vm
}

/* Free's the VM's memory. */
free_VM :: proc (vm: ^VM) {
    free_table(&vm.compiler_globals)
    free_table(&vm.globals)
    free_table(&vm.strings)
    free_objects(vm)
    delete(vm.stack)
}

/* Reads a byte from the chunk and increments the instruction pointer. */
@(private="file")
read_byte :: #force_inline proc (frame: ^CallFrame) -> byte #no_bounds_check {
    defer frame.ip = mem.ptr_offset(frame.ip, 1)
    return frame.ip^
}

/* Reads a constant from the chunk and pushes it onto the stack. */
@(private="file")
read_constant :: #force_inline proc (frame: ^CallFrame) -> Value #no_bounds_check {
    return frame.function.chunk.constants.values[read_byte(frame)]
}

@(private="file")
read_string :: #force_inline proc (frame: ^CallFrame) -> ^ObjString {
    return as_string(read_constant(frame))
}

@(private="file")
read_short :: #force_inline proc (frame: ^CallFrame) -> int #no_bounds_check {
    defer frame.ip = mem.ptr_offset(frame.ip, 2)
    return int(
        (frame.ip^ << 8) |
        (mem.ptr_offset(frame.ip, 1)^))
}

/*
Performs a binary operation on the top two values of the stack. In zen, a
binary operator can only return either a 64-bit float or a boolean. 
*/
@(private="file")
binary_op :: proc (v: ^VM, $Returns: typeid, op: string) -> InterpretResult {
    if !is_number(vm_peek(v, 0)) || !is_number(vm_peek(v, 1)) {
        vm_panic(v,
            "Expected numbers as operands to '%s', got %v and %v instead.",
            op, type_of_value(vm_peek(v, 1)), type_of_value(vm_peek(v, 0)))
        return .INTERPRET_RUNTIME_ERROR
    }

    b := as_number(vm_pop(v))
    a := as_number(vm_pop(v))

    switch typeid_of(Returns) {
        case f64:
            // Note: Addition is handled seperately from this procedure.
            switch op {
                case "-": vm_push(v, number_val(a - b))
                case "*": vm_push(v, number_val(a * b))
                case "/": {
                    if b == 0 {
                        vm_panic(v, "Cannot divide by zero.")
                        return .INTERPRET_RUNTIME_ERROR
                    }
                    vm_push(v, number_val(a / b))
                }
            }
        case bool: {
            switch op {
                    case ">": vm_push(v, bool_val(a > b))
                    case "<": vm_push(v, bool_val(a < b))
            }
        }
        case:
            fmt.eprint(COL_RED, "bug:", RESET, " ") 
            fmt.eprintf("Invalid return type for binary operation '%s'.\n", op)
            unreachable()
    }

    return nil
}

/*
Run the VM, going through the bytecode and interpreting each instruction
one by one.
*/
@(private="file")
run :: proc (vm: ^VM) -> InterpretResult #no_bounds_check {
    frame := &vm.frames[vm.frame_count - 1]

    for {
        when ODIN_DEBUG {
            offset := mem.ptr_sub(frame.ip, &frame.function.chunk.code[0])
            fmt.printf("          ")
            for value in vm.stack {
                fmt.printf("[ ")
                print_value(value)
                fmt.printf(" ]")
            }
            fmt.printf("\n")
            disassemble_instruction(&frame.function.chunk, offset)
        }

        instruction := OpCode(read_byte(frame))

        switch instruction {
            case .OP_CONSTANT:
                constant := read_constant(frame)
                vm_push(vm, constant)
            case .OP_NIL:      vm_push(vm, nil_val())
            case .OP_TRUE:     vm_push(vm, bool_val(true))
            case .OP_FALSE:    vm_push(vm, bool_val(false))
            case .OP_POP:      vm_pop(vm)
            case .OP_DUP:      vm_push(vm, vm_peek(vm, 0))
            case .OP_GET_LOCAL:
                slot := read_byte(frame)
                vm_push(vm, frame.slots[slot])
            case .OP_SET_LOCAL:
                slot := read_byte(frame)
                frame.slots[slot] = vm_peek(vm, 0)
            case .OP_GET_GLOBAL: {
                name := read_string(frame)
                value: Value; ok: bool
                if value, ok = table_get(&vm.globals, name); !ok {
                    vm_panic(vm, "Undefined variable '%s'.", name.chars)
                    return .INTERPRET_RUNTIME_ERROR
                }
                vm_push(vm, value)
            }
            case .OP_DEFINE_GLOBAL:
                name := read_string(frame)
                table_set(&vm.globals, name, vm_peek(vm, 0))
                vm_pop(vm)
            case .OP_SET_GLOBAL:
                name := read_string(frame)

                if table_set(&vm.globals, name, vm_peek(vm, 0)) {
                    table_delete(&vm.globals, name)
                    vm_panic(vm, "Undefined variable '%s'.", name.chars)
                    return .INTERPRET_RUNTIME_ERROR
                }
            case .OP_EQUAL: {
                b := vm_pop(vm)
                a := vm_pop(vm)
                vm_push(vm, bool_val(values_equal(a, b)))
            }
            case .OP_GREATER: binary_op(vm, bool, ">") or_return
            case .OP_LESS:    binary_op(vm, bool, "<") or_return
            case .OP_ADD:      
                b := vm_pop(vm)
                a := vm_pop(vm)
                if is_string(a) && is_string(b) {
                    concatenate(vm,
                        as_string(a), as_string(b))
                } else if is_number(a) && is_number(b) {
                    b := as_number(b)
                    a := as_number(a)
                    vm_push(vm, number_val(a + b))
                } else {
                    vm_panic(vm, 
                        "Expected two numbers or two strings as operands to '+', got %v and %v instead.",
                        type_of_value(a), type_of_value(b))
                    return .INTERPRET_RUNTIME_ERROR
                }
            case .OP_SUBTRACT: binary_op(vm, f64, "-") or_return
            case .OP_MULTIPLY: binary_op(vm, f64, "*") or_return
            case .OP_DIVIDE:   binary_op(vm, f64, "/") or_return
            case .OP_NOT:
                vm_push(vm, bool_val(is_falsey(vm_pop(vm))))
            case .OP_NEGATE:
                if !is_number(vm_peek(vm, 0)) {
                    vm_panic(vm, "Can only negate numbers.")
                    return .INTERPRET_RUNTIME_ERROR
                }
                vm_push(vm, number_val(-as_number(vm_pop(vm))))
            case .OP_PRINT:
                print_value(vm_pop(vm))
                fmt.printf("\n")
            case .OP_JUMP:
                offset := read_short(frame)
                frame.ip = mem.ptr_offset(frame.ip, offset)
            case .OP_JUMP_IF_FALSE:
                offset := read_short(frame)
                if is_falsey(vm_peek(vm, 0)) {
                    frame.ip = mem.ptr_offset(frame.ip, offset)
                }
            case .OP_LOOP:
                offset := read_short(frame)
                frame.ip = mem.ptr_offset(frame.ip, -offset)
            case .OP_CALL:
                arg_count := read_byte(frame)
                // Return with an error if the call fails.
                if !call_value(vm, vm_peek(vm, int(arg_count)), int(arg_count))
                {
                    return .INTERPRET_RUNTIME_ERROR
                }
                frame = &vm.frames[vm.frame_count - 1]
            // TODO: fix the function objects staying on the stack after a call
            case .OP_RETURN: {
                result := vm_pop(vm)
                vm.frame_count -= 1
                if vm.frame_count == 0 {
                    vm_pop(vm)
                    return .INTERPRET_OK
                }

                // Pop all the args and the function itself off the stack.
                for _ in 0 ..= int(frame.function.arity) {
                    vm_pop(vm)
                }
                vm_push(vm, result)
                frame = &vm.frames[vm.frame_count - 1]
            }
        }
    }
}

/* Interpret a chunk. */
interpret :: proc (vm: ^VM, source: string) -> InterpretResult {
    lexer := init_lexer(source)
    tokens, lx_ok := lex(&lexer)
    defer delete(tokens)
    if !lx_ok {
        return .INTERPRET_LEX_ERROR
    }

    fn, cmp_ok := compile(vm, tokens, &vm.compiler_globals)
    if !cmp_ok {
        return .INTERPRET_COMPILE_ERROR
    }

    vm_push(vm, obj_val(fn))
    call(vm, fn, 0) // The script itself is a function, so call it.

    return run(vm)
}

/* Push a value onto the stack. */
@(private="file")
vm_push :: #force_inline proc (vm: ^VM, value: Value) #no_bounds_check {
    append(&vm.stack, value)
}

/* Pop a value out of the stack. */
@(private="file")
vm_pop :: #force_inline proc (vm: ^VM) -> Value #no_bounds_check {
    assert(len(vm.stack) > 0)
    return pop(&vm.stack)
}

/* Peek at a certain distance from the top of the stack. */
@(private="file")
vm_peek :: #force_inline proc (vm: ^VM, distance: int) -> Value #no_bounds_check {
    return vm.stack[len(vm.stack) - 1 - distance]
}

/* Returns true if provided value is falsey. */
@(private="file")
is_falsey :: #force_inline proc (value: Value) -> bool {
    return is_nil(value) || (is_bool(value) && !as_bool(value))
}

/* 
Call a function.
Simply initializes the next CallFrame on the stack.
*/
@(private="file")
call :: proc (vm: ^VM, function: ^ObjFunction, arg_count: int) -> bool {
    if arg_count != int(function.arity) {
        vm_panic(vm, "Expected %d arguments but got %d.",
            function.arity, arg_count)
        return false
    }

    if vm.frame_count == FRAMES_MAX {
        vm_panic(vm, "Stack overflow.")
        return false
    }

    frame := &vm.frames[vm.frame_count]
    vm.frame_count += 1
    frame.function = function
    frame.ip = &function.chunk.code[0]

    // Subtract the length of the stack by the number of args, and by 1 for the
    // function itself, to get the beginning of the frame.
    frame.slots = vm.stack[len(vm.stack) - arg_count - 1:]
    return true
}

/* Call a value if its a callable, else panic. */
@(private="file")
call_value :: proc (vm: ^VM, callee: Value, arg_count: int) -> bool {
    if is_obj(callee) {
        #partial switch obj_type(callee) {
            case .FUNCTION: return call(vm, as_function(callee), arg_count)
            case .NATIVE:
                native := as_native(callee)
                result := native(
                    arg_count, 
                    vm.stack[len(vm.stack) - arg_count:])
                for _ in 0 ..= arg_count {
                    vm_pop(vm)
                }
                vm_push(vm, result)
                return true
        }
    }

    vm_panic(vm, "Can only call functions and classes.")
    return false
}

/* Concatenate two strings. */
@(private="file")
concatenate :: proc (vm: ^VM, a: ^ObjString, b: ^ObjString) {
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
