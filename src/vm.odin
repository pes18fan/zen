package zen

import "core:fmt"
import "core:math"
import "core:slice"
import "core:mem"
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
	closure: ^ObjClosure,

	/* Pointer to the current instruction in the frame. */
	ip:      ^byte,

	/* A slice of the VM's main stack. */
	slots:   ^Value,
}

/* The virtual machine that interprets the bytecode. */
VM :: struct {
	/* The chunk being interpreted. */
	chunk:            ^Chunk,

	/* The stack of values. */
	stack:            [dynamic]Value,

	/* Call frames present in the chunk. */
	frames:           [FRAMES_MAX]CallFrame,
	frame_count:      int,

	/* Table of compile-time global variables; necessary for the REPL. */
	compiler_globals: Table,

	/* Linked list of all open upvalues. */
	open_upvalues:    ^ObjUpvalue,
	gc:               ^GC,
}

/* The result of the interpreting. */
InterpretResult :: enum {
	INTERPRET_OK,
	INTERPRET_LEX_ERROR,
	INTERPRET_COMPILE_ERROR,
	INTERPRET_RUNTIME_ERROR,
}

/* Raise a runtime error. */
vm_panic :: proc(vm: ^VM, format: string, args: ..any) {
	fmt.eprintf("%spanic:%s ", COL_RED, RESET)
	fmt.eprintf("%s", fmt.tprintf(format, ..args))
	fmt.eprintln()

	for i := vm.frame_count - 1; i >= 0; i -= 1 {
		frame := &vm.frames[i]
		function := frame.closure.function
		instruction := mem.ptr_sub(frame.ip, &function.chunk.code[0]) - 1
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

/* 
Define a native function.
This assumes that gc.mark_roots_arg is a pointer to the VM. This assumption
is not harmful, as this function is never called and never needs to be called
during compilation of the source.
*/
define_native :: proc(gc: ^GC, name: string, function: NativeFn, arity: int) {
	vm := as_vm(gc.mark_roots_arg)
	vm_push(vm, obj_val(copy_string(gc, name)))
	vm_push(vm, obj_val(new_native(gc, function, arity)))
	table_set(&gc.globals, as_string(vm.stack[0]), vm.stack[1])
	vm_pop(vm)
	vm_pop(vm)
}

/* Resets the stack. */
reset_stack :: proc(vm: ^VM) {
	defer {
		delete(vm.stack)
		vm.stack = make([dynamic]Value, 0, 0)
	}
	vm.frame_count = 0
	vm.open_upvalues = nil
}

/* Returns a newly created VM. */
init_VM :: proc() -> VM {
	vm := VM {
		chunk            = nil,
		stack            = make([dynamic]Value, 0, 0),
		open_upvalues    = nil,
		compiler_globals = init_table(),
		frame_count      = 0,
	}

	return vm
}

/* Free's the VM's memory. */
free_VM :: proc(vm: ^VM) {
	free_table(&vm.compiler_globals)
	delete(vm.stack)
}

/* Reads a byte from the chunk and increments the instruction pointer. */
@(private = "file")
read_byte :: #force_inline proc(frame: ^CallFrame) -> byte #no_bounds_check {
	defer frame.ip = mem.ptr_offset(frame.ip, 1)
	return frame.ip^
}

/* Reads a constant from the chunk and pushes it onto the stack. */
@(private = "file")
read_constant :: #force_inline proc(frame: ^CallFrame) -> Value #no_bounds_check {
	return frame.closure.function.chunk.constants.values[read_byte(frame)]
}

@(private = "file")
read_string :: #force_inline proc(frame: ^CallFrame) -> ^ObjString {
	return as_string(read_constant(frame))
}

@(private = "file")
read_short :: #force_inline proc(frame: ^CallFrame) -> int #no_bounds_check {
	defer frame.ip = mem.ptr_offset(frame.ip, 2)
	return int((frame.ip^ << 8) | (mem.ptr_offset(frame.ip, 1)^))
}

/*
Performs a binary operation on the top two values of the stack. In zen, a
binary operator can only return either a 64-bit float or a boolean. 
*/
@(private = "file")
binary_op :: proc(v: ^VM, $Returns: typeid, op: string) -> InterpretResult {
	if !is_number(vm_peek(v, 0)) || !is_number(vm_peek(v, 1)) {
		vm_panic(
			v,
			"Expected numbers as operands to '%s', got %v and %v instead.",
			op,
			type_of_value(vm_peek(v, 1)),
			type_of_value(vm_peek(v, 0)),
		)
		return .INTERPRET_RUNTIME_ERROR
	}

	b := as_number(vm_pop(v))
	a := as_number(vm_pop(v))

	switch typeid_of(Returns) {
	case f64:
		// Note: Addition is handled seperately from this procedure.
		switch op {
		case "-":
			vm_push(v, number_val(a - b))
		case "*":
			vm_push(v, number_val(a * b))
		case "/":
			{
				if b == 0 {
					vm_panic(v, "Cannot divide by zero.")
					return .INTERPRET_RUNTIME_ERROR
				}
				vm_push(v, number_val(a / b))
			}
		}
	case bool:
		{
			switch op {
			case ">":
				vm_push(v, bool_val(a > b))
			case "<":
				vm_push(v, bool_val(a < b))
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
@(private = "file")
run :: proc(vm: ^VM) -> InterpretResult #no_bounds_check {
	frame := &vm.frames[vm.frame_count - 1]

	/* This variable stores the value of the built-in `it` variable, which
	refers to the value of the last expression in a pipeline. */
	pipeline_it: Value = nil

	for {
		if config.trace_exec {
			fmt.printf("          ")
			for value in vm.stack {
				fmt.printf("[ ")
				print_value(value)
				fmt.printf(" ]")
			}
			fmt.printf("\n")
			offset := mem.ptr_sub(frame.ip, &frame.closure.function.chunk.code[0])
			disassemble_instruction(&frame.closure.function.chunk, offset)
		}

		instruction := OpCode(read_byte(frame))

		switch instruction {
		case .OP_NOOP:
			// Do nothing.
		case .OP_CONSTANT:
			constant := read_constant(frame)
			vm_push(vm, constant)
		case .OP_NIL:
			vm_push(vm, nil_val())
		case .OP_TRUE:
			vm_push(vm, bool_val(true))
		case .OP_FALSE:
			vm_push(vm, bool_val(false))
		case .OP_POP:
			vm_pop(vm)
		case .OP_DUP:
			vm_push(vm, vm_peek(vm, 0))
		case .OP_GET_LOCAL:
			slot := read_byte(frame)
			vm_push(vm, mem.ptr_offset(frame.slots, slot)^)
		case .OP_SET_LOCAL:
			slot := read_byte(frame)
			mem.ptr_offset(frame.slots, slot)^ = vm_peek(vm, 0)
		case .OP_GET_GLOBAL:
			{
				name := read_string(frame)
				value: Value;ok: bool
				if value, ok = table_get(&vm.gc.globals, name); !ok {
					vm_panic(vm, "Undefined variable '%s'.", name.chars)
					return .INTERPRET_RUNTIME_ERROR
				}
				vm_push(vm, value)
			}
		case .OP_DEFINE_GLOBAL:
			name := read_string(frame)
			table_set(&vm.gc.globals, name, vm_peek(vm, 0))
			vm_pop(vm)
		case .OP_SET_GLOBAL:
			{
				name := read_string(frame)

				if table_set(&vm.gc.globals, name, vm_peek(vm, 0)) {
					table_delete(&vm.gc.globals, name)
					vm_panic(vm, "Undefined variable '%s'.", name.chars)
					return .INTERPRET_RUNTIME_ERROR
				}
			}
		case .OP_GET_UPVALUE:
			{
				slot := read_byte(frame)
				vm_push(vm, frame.closure.upvalues[slot].location^)
			}
		case .OP_SET_UPVALUE:
			{
				slot := read_byte(frame)
				// Take the value on top of the stack and store it into the slot.
				frame.closure.upvalues[slot].location^ = vm_peek(vm, 0)
			}
		case .OP_GET_PROPERTY:
		    {
				if !is_instance(vm_peek(vm, 0)) {
					vm_panic(vm, "Only instances have properties.")
					return .INTERPRET_RUNTIME_ERROR
				}

				instance := as_instance(vm_peek(vm, 0))
				name := read_string(frame)

				value: Value; ok: bool
				if value, ok = table_get(&instance.fields, name); !ok {
					vm_panic(vm, "Undefined property '%s'.", name.chars)
					return .INTERPRET_RUNTIME_ERROR
				}

				vm_pop(vm) /* Instance. */
				vm_push(vm, value)
			}
		case .OP_SET_PROPERTY:
			{
				if !is_instance(vm_peek(vm, 1)) {
					vm_panic(vm, "Only instances have fields.")
					return .INTERPRET_RUNTIME_ERROR
				}

				/* Get the instance, which is at this moment the 2nd to the top
				 * value on the stack. */
				instance := as_instance(vm_peek(vm, 1))

				/* Store the value on top of the stack into the instance's fields. */
				table_set(&instance.fields, read_string(frame), vm_peek(vm, 0))

				/* Pop the value that we just stored as a field. */
				value := vm_pop(vm)

				/* Pop the instance off the stack. */
				vm_pop(vm)

				/* Push the value we stored back on the stack, that's what a 
				 * setter expression does. */
				vm_push(vm, value)
			}
		case .OP_GET_IT:
			vm_push(vm, pipeline_it)
		case .OP_SET_IT:
			pipeline_it = vm_pop(vm)
		case .OP_EQUAL:
			{
				b := vm_pop(vm)
				a := vm_pop(vm)
				vm_push(vm, bool_val(values_equal(a, b)))
			}
		case .OP_GREATER:
			binary_op(vm, bool, ">") or_return
		case .OP_LESS:
			binary_op(vm, bool, "<") or_return
		case .OP_ADD:
			if is_string(vm_peek(vm, 0)) && is_string(vm_peek(vm, 1)) {
				concatenate(vm)
			} else if is_number(vm_peek(vm, 0)) && is_number(vm_peek(vm, 1)) {
				b := as_number(vm_pop(vm))
				a := as_number(vm_pop(vm))
				vm_push(vm, number_val(a + b))
			} else if is_complex(vm_peek(vm, 0)) || is_complex(vm_peek(vm, 1)) {
				b: complex128; a: complex128

				if is_number(vm_peek(vm, 0)) {
					b = complex128(as_number(vm_pop(vm)))
					a = as_complex(vm_pop(vm))
					vm_push(vm, complex_val(a + b))
				} else if is_number(vm_peek(vm, 1)) {
					b = as_complex(vm_pop(vm))
					a = complex128(as_number(vm_pop(vm)))
					vm_push(vm, complex_val(a + b))
				} else if is_complex(vm_peek(vm, 0)) && is_complex(vm_peek(vm, 1)) {
					b = as_complex(vm_pop(vm))
					a = as_complex(vm_pop(vm))
					vm_push(vm, complex_val(a + b))
				} else {
					vm_panic(
						vm,
						"Expected two numbers or two strings as operands to '+', got %v and %v instead.",
						type_of_value(vm_pop(vm)),
						type_of_value(vm_pop(vm)),
					)
					return .INTERPRET_RUNTIME_ERROR
				}
			} else {
				vm_panic(
					vm,
					"Expected two numbers or two strings as operands to '+', got %v and %v instead.",
					type_of_value(vm_pop(vm)),
					type_of_value(vm_pop(vm)),
				)
				return .INTERPRET_RUNTIME_ERROR
			}
		case .OP_SUBTRACT:
			if is_complex(vm_peek(vm, 0)) || is_complex(vm_peek(vm, 1)) {
				b: complex128; a: complex128

				if is_number(vm_peek(vm, 0)) {
					b = complex128(as_number(vm_pop(vm)))
					a = as_complex(vm_pop(vm))
					vm_push(vm, complex_val(a - b))
				} else if is_number(vm_peek(vm, 1)) {
					b = as_complex(vm_pop(vm))
					a = complex128(as_number(vm_pop(vm)))
					vm_push(vm, complex_val(a - b))
				} else if is_complex(vm_peek(vm, 0)) && is_complex(vm_peek(vm, 1)) {
					b = as_complex(vm_pop(vm))
					a = as_complex(vm_pop(vm))
					vm_push(vm, complex_val(a - b))
				} else {
					vm_panic(
						vm,
						"Expected two numbers as operands to '-', got %v and %v instead.",
						type_of_value(vm_pop(vm)),
						type_of_value(vm_pop(vm)),
					)
					return .INTERPRET_RUNTIME_ERROR
				}
			} else {
				binary_op(vm, f64, "-") or_return
			}
			case .OP_MULTIPLY:
			if is_complex(vm_peek(vm, 0)) || is_complex(vm_peek(vm, 1)) {
				b: complex128; a: complex128

				if is_number(vm_peek(vm, 0)) {
					b = complex128(as_number(vm_pop(vm)))
					a = as_complex(vm_pop(vm))
					vm_push(vm, complex_val(a * b))
				} else if is_number(vm_peek(vm, 1)) {
					b = as_complex(vm_pop(vm))
					a = complex128(as_number(vm_pop(vm)))
					vm_push(vm, complex_val(a * b))
				} else if is_complex(vm_peek(vm, 0)) && is_complex(vm_peek(vm, 1)) {
					b = as_complex(vm_pop(vm))
					a = as_complex(vm_pop(vm))
					vm_push(vm, complex_val(a * b))
				} else {
					vm_panic(
						vm,
						"Expected two numbers as operands to '*', got %v and %v instead.",
						type_of_value(vm_pop(vm)),
						type_of_value(vm_pop(vm)),
					)
					return .INTERPRET_RUNTIME_ERROR
				}
			} else {
				binary_op(vm, f64, "*") or_return
			}
			case .OP_DIVIDE:
			if is_complex(vm_peek(vm, 0)) || is_complex(vm_peek(vm, 1)) {
				b: complex128; a: complex128

				if is_number(vm_peek(vm, 0)) {
					b = complex128(as_number(vm_pop(vm)))
					a = as_complex(vm_pop(vm))
					vm_push(vm, complex_val(a / b))
				} else if is_number(vm_peek(vm, 1)) {
					b = as_complex(vm_pop(vm))
					a = complex128(as_number(vm_pop(vm)))
					vm_push(vm, complex_val(a / b))
				} else if is_complex(vm_peek(vm, 0)) && is_complex(vm_peek(vm, 1)) {
					b = as_complex(vm_pop(vm))
					a = as_complex(vm_pop(vm))
					vm_push(vm, complex_val(a / b))
				} else {
					vm_panic(
						vm,
						"Expected two numbers as operands to '/', got %v and %v instead.",
						type_of_value(vm_pop(vm)),
						type_of_value(vm_pop(vm)),
					)
					return .INTERPRET_RUNTIME_ERROR
				}
			} else {
				binary_op(vm, f64, "/") or_return
			}
			case .OP_NOT:
			vm_push(vm, bool_val(is_falsey(vm_pop(vm))))
			case .OP_NEGATE:
			if !is_number(vm_peek(vm, 0)) && !is_complex(vm_peek(vm, 0)) {
				vm_panic(vm, "Can only negate numbers.")
				return .INTERPRET_RUNTIME_ERROR
			}

			if is_number(vm_peek(vm, 0)) {
				vm_push(vm, number_val(-as_number(vm_pop(vm))))
			} else {
				vm_push(vm, complex_val(-as_complex(vm_pop(vm))))
			}
		case .OP_PRINT:
			print_value(vm_pop(vm))
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
			{
				arg_count := read_byte(frame)
				// Return with an error if the call fails.
				if !call_value(vm, vm_peek(vm, int(arg_count)), int(arg_count)) {
					return .INTERPRET_RUNTIME_ERROR
				}
				frame = &vm.frames[vm.frame_count - 1]
			}
		case .OP_LIST:
			{
				item_count := read_byte(frame)

				list := new_list(vm.gc)

				for i := 0; i < int(item_count); i += 1 {
					write_value_array(&list.items, vm_pop(vm))
				}

				slice.reverse(list.items.values[:])

				vm_push(vm, obj_val(list))
			}
		case .OP_SUBSCRIPT:
			{
				b := vm_pop(vm)
				a := vm_pop(vm)

				if !is_list(a) {
					vm_panic(vm, "Can only subscript lists.")
					return .INTERPRET_RUNTIME_ERROR
				}

				if !is_number(b) {
					vm_panic(vm, "List index must be a positive integer.")
					return .INTERPRET_RUNTIME_ERROR
				}

				index := as_number(b)
				list := as_list(a)

				if math.floor(index) != index || index < 0 {
					vm_panic(vm, "List index must be a positive integer.")
					return .INTERPRET_RUNTIME_ERROR
				}

				if int(index) >= list.items.count {
					vm_panic(vm, "Index out of bounds.")
					return .INTERPRET_RUNTIME_ERROR
				}

				vm_push(vm, list.items.values[int(index)])
			}
		case .OP_CLOSURE:
			{
				function := as_function(read_constant(frame))
				closure := new_closure(vm.gc, function)
				vm_push(vm, obj_val(closure))

				for i in 0 ..< closure.upvalue_count {
					is_local := bool(read_byte(frame))
					index := read_byte(frame)

					if is_local {
						// Close over a local var of the surrounding function.
						closure.upvalues[i] = capture_upvalue(vm, mem.ptr_offset(frame.slots, index))
					} else {
						// Capture an upvalue from the surrounding function.
						/* When the OP_CLOSURE instruction is being executed,
						the surrounding function of that closure is at the top
						of the callstack, so we can just pick it up from the
						current CalLFrame. */
						closure.upvalues[i] = frame.closure.upvalues[index]
					}
				}
			}
		case .OP_CLOSE_UPVALUE:
			{
				close_upvalues(vm, &vm.stack[len(vm.stack) - 1])
				vm_pop(vm)
			}
		case .OP_RETURN:
			{
				result := vm_pop(vm) // Retrieve the return value from the stack.

				// Close any upvalues that were captured inside the returning function.
				close_upvalues(vm, frame.slots)
				vm.frame_count -= 1

				if vm.frame_count == 0 {
					vm_pop(vm)
					return .INTERPRET_OK
				}

				/* Pop off all of the local variables and arguments of the
				 * function. */
				#reverse for value in vm.stack {
					if values_equal(value, frame.slots^) {
						break
					}
					vm_pop(vm)
				}
				vm_pop(vm) // Pop the function itself.

				vm_push(vm, result) // Push the return value back to the stack.
				frame = &vm.frames[vm.frame_count - 1]
			}
		case .OP_CLASS:
			{
				vm_push(vm, obj_val(new_class(vm.gc, read_string(frame))))
			}
		}
	}
}

/* Interpret a chunk. */
interpret :: proc(vm: ^VM, gc: ^GC, source: string) -> InterpretResult {
	/* Start the stopwatch. */
	sw: time.Stopwatch
	if config.record_time {
		time.stopwatch_start(&sw)
	}

	lexer := init_lexer(source)
	tokens, lx_ok := lex(&lexer)
	defer delete(tokens)
	if !lx_ok {
		return .INTERPRET_LEX_ERROR
	}

	/* Time the lexer. */
	if config.record_time {
		time.stopwatch_stop(&sw)
		fmt.eprintf("Lexer: %v\n", time.stopwatch_duration(sw))
		time.stopwatch_reset(&sw)
		time.stopwatch_start(&sw)
	}

	fn, cmp_ok := compile(gc, tokens, &vm.compiler_globals)
	if !cmp_ok {
		return .INTERPRET_COMPILE_ERROR
	}

	/* Time the compiler. */
	if config.record_time {
		time.stopwatch_stop(&sw)
		fmt.eprintf("Compiler: %v\n", time.stopwatch_duration(sw))
		time.stopwatch_reset(&sw)
		time.stopwatch_start(&sw)
	}

	/* Time the VM. */
	defer if config.record_time {
		time.stopwatch_stop(&sw)
		fmt.eprintf("\nVM: %v\n", time.stopwatch_duration(sw))
	}

	/* If the user only wants to compile the script, then we can stop here. */
	if config.compile_only {
		return .INTERPRET_OK
	}

	vm_push(vm, obj_val(fn))
	closure := new_closure(gc, fn)
	vm_pop(vm)
	vm_push(vm, obj_val(closure))
	call(vm, closure, 0) // The script itself is a function, so call it.

	return run(vm)
}

/* Push a value onto the stack. */
vm_push :: #force_inline proc(vm: ^VM, value: Value) #no_bounds_check {
	append(&vm.stack, value)
}

/* Pop a value out of the stack. */
vm_pop :: #force_inline proc(vm: ^VM) -> Value #no_bounds_check {
	assert(len(vm.stack) > 0)
	return pop(&vm.stack)
}

/* Peek at a certain distance from the top of the stack. */
vm_peek :: #force_inline proc(vm: ^VM, distance: int) -> Value #no_bounds_check {
	return vm.stack[len(vm.stack) - 1 - distance]
}

/* Returns true if provided value is falsey. */
@(private = "file")
is_falsey :: #force_inline proc(value: Value) -> bool {
	return is_nil(value) || (is_bool(value) && !as_bool(value))
}

/* 
Call a function.
Simply initializes the next CallFrame on the stack.
*/
@(private = "file")
call :: proc(vm: ^VM, closure: ^ObjClosure, arg_count: int) -> bool {
	if arg_count != int(closure.function.arity) {
		vm_panic(vm, "Expected %d arguments but got %d.", closure.function.arity, arg_count)
		return false
	}

	if vm.frame_count == FRAMES_MAX {
		vm_panic(vm, "Stack overflow.")
		return false
	}

	frame := &vm.frames[vm.frame_count]
	vm.frame_count += 1
	frame.closure = closure
	frame.ip = &closure.function.chunk.code[0]

	// Subtract the length of the stack by the number of args, and by 1 for the
	// function itself, to get the beginning of the frame.
	frame.slots = &vm.stack[len(vm.stack) - arg_count - 1]
	return true
}

/* Call a value if its a callable, else panic. */
@(private = "file")
call_value :: proc(vm: ^VM, callee: Value, arg_count: int) -> (success: bool) {
	if is_obj(callee) {
		#partial switch obj_type(callee) {
		case .CLASS: {
			klass := as_class(callee)
			vm.stack[len(vm.stack) - arg_count - 1] = obj_val(new_instance(vm.gc, klass))
			return true
		}
		/* We only handle ObjClosures here, since all ObjFunctions are wrapped
		into closures as soon as they're pulled out of the constant table. */
		case .CLOSURE:
			return call(vm, as_closure(callee), arg_count)
		case .NATIVE:
			if arg_count != as_native_obj(callee).arity {
				vm_panic(
					vm,
					"Expected %d arguments but got %d.",
					as_native_obj(callee).arity,
					arg_count,
				)
				return false
			}
			native := as_native(callee)

			result, ok := native(vm, arg_count, vm.stack[len(vm.stack) - arg_count:])

			if !ok {
				return false
			}

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

/* Capture the provided stack slot as an upvalue. */
@(private = "file")
capture_upvalue :: proc(vm: ^VM, local: ^Value) -> ^ObjUpvalue {
	prev_upvalue: ^ObjUpvalue = nil
	upvalue := vm.open_upvalues

	// Look for an existing upvalue before creating a new one.
	for upvalue != nil && upvalue.location > local {
		prev_upvalue = upvalue
		upvalue = upvalue.next_upvalue
	}

	// Return it if it already exists.
	if upvalue != nil && upvalue.location == local {
		return upvalue
	}

	created_upvalue := new_upvalue(vm.gc, local)
	created_upvalue.next_upvalue = upvalue

	if prev_upvalue == nil {
		vm.open_upvalues = created_upvalue
	} else {
		prev_upvalue.next_upvalue = created_upvalue
	}

	return created_upvalue
}

/* Close all upvalues up to and including the provided stack slot. */
@(private = "file")
close_upvalues :: proc(vm: ^VM, last: ^Value) {
	for vm.open_upvalues != nil && vm.open_upvalues.location >= last {
		upvalue := vm.open_upvalues
		upvalue.closed = upvalue.location^
		upvalue.location = &upvalue.closed
		vm.open_upvalues = upvalue.next_upvalue
	}
}

/* Concatenate two strings. */
@(private = "file")
concatenate :: proc(vm: ^VM) {
	/* A collection may occur when concatenating. To prevent the GC from
	collecting the strings being concatenated, we just peek the strings from the
	heap, concatenate them and then only pop them off. */
	b := as_string(vm_peek(vm, 0))
	a := as_string(vm_peek(vm, 1))

	length := a.len + b.len
	chars := make([]byte, length)
	i := 0
	i = +copy(chars, a.chars)
	copy(chars[i:], b.chars)

	result := take_string(vm.gc, string(chars))
	/* Pop off the two original strings. */
	vm_pop(vm)
	vm_pop(vm)

	/* And push the final result. */
	vm_push(vm, (^Obj)(result))
}

free_objects :: proc(gc: ^GC) {
	object := gc.objects

	for object != nil {
		next := object.next
		free_object(gc, object)
		object = next
	}
}
