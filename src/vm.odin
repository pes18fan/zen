package zen

import "core:fmt"
import "core:math"
import "core:mem"
import "core:os"
import "core:reflect"
import "core:slice"
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
	closure: ^ObjClosure,

	/* Pointer to the current instruction in the frame. */
	ip:      ^byte,

	/* A slice of the VM's main stack. */
	slots:   ^Value,
}

/* The virtual machine that interprets the bytecode. */
VM :: struct {
	/* The path of the running program. Used to distinguish programs since they
     * can import each other as modules, and so it is necessary to disallow
     * cyclic imports. Value is the file path for a file and an empty string
     * for a REPL, since you can't import REPLs. */
	path:             string,

	/* Just the path, except with the file extension and other path stuff stripped
     * out, like in a module name. For instance, for a path "a/b/c.zn", the name
     * will be just "c". Used for working with user-defined modules, but NOT
     * for distinguishing them. */
	name:             string,

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
	INTERPRET_READ_ERROR,
	INTERPRET_RUNTIME_ERROR,
}

/* Raise a runtime error. */
vm_panic :: proc(vm: ^VM, format: string, args: ..any) {
	color_red(os.stderr, "panic: ")
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

	color_yellow(os.stderr, "  (at")
	if vm.path == "REPL" {
		color_yellow(os.stderr, " REPL)\n")
	} else {
		color_yellow(os.stderr, fmt.tprintf(" file %s)\n", vm.path))
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

/*
Defines a builtin module by fetching the specific functions present in
the module and adding them to the module Obj.
*/
define_builtin_module :: proc(gc: ^GC, name: string, module: BuiltinModule) {
	vm := as_vm(gc.mark_roots_arg)

	obj_module := new_module(gc, copy_string(gc, name))
	vm_push(vm, obj_val(copy_string(gc, name)))
	vm_push(vm, obj_val(obj_module))
	table_set(&gc.globals, as_string(vm.stack[0]), vm.stack[1])

	vm_pop(vm)
	vm_pop(vm)

	module_functions := get_builtin_module(gc, module)
	defer delete(module_functions)

	for function in module_functions {
		vm_push(vm, obj_val(copy_string(gc, function.name)))
		vm_push(vm, obj_val(new_native(gc, function.function, function.arity)))

		table_set(&obj_module.values, as_string(vm.stack[1]), vm.stack[2])

		vm_pop(vm)
		vm_pop(vm)
	}

	vm_push(vm, obj_val(obj_module))
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
		name             = "",
		path             = "",
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
		color_red(os.stderr, "bug: ")
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
run :: proc(vm: ^VM, importer: ImportingModule = nil) -> InterpretResult #no_bounds_check {
	frame := &vm.frames[vm.frame_count - 1]

	/* This variable stores the value of the built-in `it` variable, which
	refers to the value of the last expression in a pipeline. */
	pipeline_it: Value = nil_val()

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

				/* No runtime check is done for variable existence since that is
                 * done at compile time. */
				value, _ := table_get(&vm.gc.globals, name)
				vm_push(vm, value)
			}
		case .OP_DEFINE_GLOBAL:
			name := read_string(frame)
			table_set(&vm.gc.globals, name, vm_peek(vm, 0))
			vm_pop(vm)
		case .OP_SET_GLOBAL:
			{
				name := read_string(frame)

				/* No runtime check is done for variable existence since that is
                 * done at compile time. */
				table_set(&vm.gc.globals, name, vm_peek(vm, 0))
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
		/* This opcode is used both to get properties of an instance and to get
         * values in a module. */
		case .OP_GET_PROPERTY:
			{
				if is_module(vm_peek(vm, 0)) {
					module := as_module(vm_peek(vm, 0))
					name := read_string(frame)

					/* Look for the value in the module. */
					value: Value;ok: bool
					if value, ok = table_get(&module.values, name); ok {
						vm_pop(vm) /* Module. */
						vm_push(vm, value)
						break /* Step out of the switch statement. */
					} else {
						panic_str := fmt.tprintf(
							`Value '%s' does not exist on module '%s'.
       If this module is a file, you may have forgotten the pub keyword.`,
							name.chars,
							module.name.chars,
						)
						vm_panic(vm, panic_str)
						return .INTERPRET_RUNTIME_ERROR
					}
				} else if is_instance(vm_peek(vm, 0)) {
					instance := as_instance(vm_peek(vm, 0))
					name := read_string(frame)

					/* Look for a field. */
					value: Value;ok: bool
					if value, ok = table_get(&instance.fields, name); ok {
						vm_pop(vm) /* Instance. */
						vm_push(vm, value)
						break /* Step out of the switch statement. */
					}

					/* Look for a method. If we don't find one, it means that name
				     * wasn't a field either, which is a runtime error. */
					if !bind_method(vm, instance.klass, name) {
						return .INTERPRET_RUNTIME_ERROR
					}
				} else {
					vm_panic(vm, "Only instances have properties.")
					return .INTERPRET_RUNTIME_ERROR
				}
			}
		case .OP_SET_PROPERTY:
			{
				if is_module(vm_peek(vm, 1)) {
					vm_panic(vm, "Cannot change the values of a module.")
					return .INTERPRET_RUNTIME_ERROR
				}

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
		case .OP_GET_SUPER:
			{
				name := read_string(frame)
				superclass := as_class(vm_pop(vm))

				/* Look up the method in the superclass and push it to the
				 * stack as an ObjBoundMethod. */
				if !bind_method(vm, superclass, name) {
					return .INTERPRET_RUNTIME_ERROR
				}
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
			binary_op(vm, f64, "-") or_return
		case .OP_MULTIPLY:
			binary_op(vm, f64, "*") or_return
		case .OP_DIVIDE:
			binary_op(vm, f64, "/") or_return
		case .OP_NOT:
			vm_push(vm, bool_val(is_falsey(vm_pop(vm))))
		case .OP_NEGATE:
			{
				if !is_number(vm_peek(vm, 0)) {
					vm_panic(vm, "Can only negate numbers.")
					return .INTERPRET_RUNTIME_ERROR
				}

				vm_push(vm, number_val(-as_number(vm_pop(vm))))
			}
		case .OP_PRINT:
			print_value(vm_pop(vm))
		case .OP_JUMP:
			{
				offset := read_short(frame)
				frame.ip = mem.ptr_offset(frame.ip, offset)
			}
		case .OP_JUMP_IF_FALSE:
			{
				offset := read_short(frame)
				if is_falsey(vm_peek(vm, 0)) {
					frame.ip = mem.ptr_offset(frame.ip, offset)
				}
			}
		case .OP_LOOP:
			{
				offset := read_short(frame)
				frame.ip = mem.ptr_offset(frame.ip, -offset)
			}
		case .OP_CALL:
			{
				arg_count := read_byte(frame)

				// Return with an error if the call fails.
				if !call_value(vm, vm_peek(vm, int(arg_count)), int(arg_count)) {
					return .INTERPRET_RUNTIME_ERROR
				}

				frame = &vm.frames[vm.frame_count - 1]
			}
		case .OP_INVOKE:
			{
				method := read_string(frame)
				arg_count := read_byte(frame)

				if !invoke(vm, method, int(arg_count)) {
					return .INTERPRET_RUNTIME_ERROR
				}

				frame = &vm.frames[vm.frame_count - 1]
			}
		case .OP_SUPER_INVOKE:
			{
				method := read_string(frame)
				arg_count := read_byte(frame)
				superclass := as_class(vm_pop(vm))

				if !invoke_from_class(vm, superclass, method, int(arg_count)) {
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

				/* The list needs to be reversed since the list elements
				 * were popped off the stack in reverse order. */
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
					vm_panic(vm, "List index must be a non-negative integer.")
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

				is_public := bool(read_byte(frame))

				for i in 0 ..< closure.upvalue_count {
					is_local := bool(read_byte(frame))
					index := read_byte(frame)

					if is_local {
						// Close over a local var of the surrounding function.
						closure.upvalues[i] = capture_upvalue(
							vm,
							mem.ptr_offset(frame.slots, index),
						)
					} else {
						// Capture an upvalue from the surrounding function.
						/* When the OP_CLOSURE instruction is being executed,
						the surrounding function of that closure is at the top
						of the callstack, so we can just pick it up from the
						current CalLFrame. */
						closure.upvalues[i] = frame.closure.upvalues[index]
					}
				}

				/* If the current file is being imported AND the function being
                 * compiled is set as public with the `pub` keyword, add the 
                 * declared closure into the module that's importing it. */
				importing_module, ok := importer.(ImportingModuleStruct)
				if is_public && ok {
					module := importing_module.module
					table_set(&module.values, closure.function.name, vm_peek(vm, 0))
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
				public := bool(read_byte(frame))
				name := read_string(frame)

				vm_push(vm, obj_val(new_class(vm.gc, name)))

				/* If the current file is being imported AND the class being
                 * compiled is set as public with the `pub` keyword, add the 
                 * declared class into the module that's importing it. */
				importing_module, ok := importer.(ImportingModuleStruct)
				if public && ok {
					module := importing_module.module
					table_set(&module.values, name, vm_peek(vm, 0))
				}
			}
		case .OP_INHERIT:
			{
				superclass := vm_peek(vm, 1)

				if !is_class(superclass) {
					vm_panic(vm, "Superclass must be a class.")
					return .INTERPRET_RUNTIME_ERROR
				}

				subclass := as_class(vm_peek(vm, 0))

				/* Copy all the superclass's methods to the subclass.
				 * This is what we call "copy-down inheritance". */
				table_add_all(from = &as_class(superclass).methods, to = &subclass.methods)
				vm_pop(vm) /* Subclass. */
			}
		case .OP_METHOD:
			{
				define_method(vm, read_string(frame))
			}
		case .OP_MODULE_BUILTIN:
			{
				module_str := strings.to_upper(read_string(frame).chars)
				module, ok := reflect.enum_from_name(BuiltinModule, module_str)
				if !ok {
					vm_panic(vm, "Unknown builtin module %s.", module_str)
				}

				define_builtin_module(vm.gc, strings.to_lower(module_str), module)
			}
		case .OP_MODULE_USER:
			{
				module_name := read_string(frame)
				module_path := read_string(frame)

				/* Add a new module onto the stack. */
				module := new_module(vm.gc, module_name)

				/* Create a new VM for the imported module. */
				mod_vm := init_VM()
				defer free_VM(&mod_vm)

				mod_vm.gc = vm.gc

				prev_mark_roots := vm
				vm.gc.mark_roots_arg = &mod_vm
				defer vm.gc.mark_roots_arg = prev_mark_roots

				// run the VM on the file
				result := run_file(
					&mod_vm,
					module_path.chars,
					importer = ImportingModuleStruct {
						path = vm.path,
						name = vm.name,
						module = module,
					},
				)
				if result != .INTERPRET_OK {
					return result /* Return the errored program back out */
				}

				pop(&vm.gc.import_stack) /* Remove the path from the import stack. */

				vm_push(vm, obj_val(module))
			}
		}
	}
}

/* Interpret a chunk. */
interpret :: proc(
	vm: ^VM,
	gc: ^GC,
	source: string,
	importer: ImportingModule = nil,
) -> InterpretResult {
	/* If the name of the VM and the importing module are both the same (if the
     * importing module is not nil), then we have a cyclic import, which causes
     * all sorts of problems. So we have to disallow that. */
	importer_struct, importer_exists := importer.(ImportingModuleStruct)

	if importer_exists && slice.contains(vm.gc.import_stack[:], vm.path) {
		vm_panic(vm, "Cannot perform a cyclic import.")
		return .INTERPRET_RUNTIME_ERROR
	}

	append(&vm.gc.import_stack, vm.path)

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

	if config.dump_tokens {
		return .INTERPRET_OK
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

	return run(vm, importer)
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
		case .BOUND_METHOD:
			{
				bound := as_bound_method(callee)

				/* The receiver must be stored at stack slot zero. */
				vm.stack[len(vm.stack) - arg_count - 1] = bound.receiver

				/* Pull the raw closure out of the ObjBoundMethod and call it. */
				return call(vm, bound.method, arg_count)
			}
		case .CLASS:
			{
				klass := as_class(callee)
				vm.stack[len(vm.stack) - arg_count - 1] = obj_val(new_instance(vm.gc, klass))

				/* Look for an initializer. */
				initializer: Value;ok: bool
				if initializer, ok = table_get(&klass.methods, vm.gc.init_string); ok {
					return call(vm, as_closure(initializer), arg_count)
				} else if arg_count != 0 {
					/* If there is no initializer, passing arguments to a class
				 * call makes no sense and is thus an error. */
					vm_panic(vm, "Expected 0 arguments but got %d.", arg_count)
					return false
				}

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

@(private = "file")
invoke_from_class :: proc(vm: ^VM, klass: ^ObjClass, name: ^ObjString, arg_count: int) -> bool {
	method: Value;ok: bool
	if method, ok = table_get(&klass.methods, name); !ok {
		vm_panic(vm, "Undefined property '%s'.", name.chars)
		return false
	}

	return call(vm, as_closure(method), arg_count)
}

/* Invoke a method or a function in a module. */
@(private = "file")
invoke :: proc(vm: ^VM, name: ^ObjString, arg_count: int) -> bool {
	receiver := vm_peek(vm, arg_count)

	if is_module(receiver) {
		module := as_module(receiver)

		value: Value;ok: bool

		if value, ok = table_get(&module.values, name); ok {
			args := make([dynamic]Value)
			defer delete(args)

			for i in 0 ..< int(arg_count) {
				append(&args, vm_pop(vm)) /* Temporarily pop off all the args. */
			}

			vm_pop(vm) /* Module. */
			vm_push(vm, value) /* Push the invoked function on the stack. */

			#reverse for a in args {
				vm_push(vm, a) /* Push back all the args on the stack. */
			}

			return call_value(vm, vm_peek(vm, int(arg_count)), int(arg_count))
		} else {
			panic_str := fmt.tprintf(
				`Value '%s' does not exist on module '%s'.
       If this module is a file, you may have forgotten the pub keyword.`,
				name.chars,
				module.name.chars,
			)
			vm_panic(vm, panic_str)
			return false
		}
	}

	if !is_instance(receiver) {
		vm_panic(vm, "Only methods and module functions can be invoked.")
		return false
	}

	instance := as_instance(receiver)

	/* We've reached this point since the compiler thought that it saw a
	 * method invocation. However, fields on instances can also contain 
	 * functions, so it might have actually seen a field access and an immediate
	 * call of the function stored in that field. To handle this corner case,
	 * we need to look for a field of the same name in that instance first.
	 * This is necessary but unfortunately sacrifices a bit of performance. */
	value: Value;ok: bool
	if value, ok = table_get(&instance.fields, name); ok {
		/* Replace the receiver under the arguments with the value of the
		 * field, since the function itself is always the first value in
		 * its callframe. */
		vm.stack[len(vm.stack) - arg_count - 1] = value
		return call_value(vm, value, arg_count)
	}

	return invoke_from_class(vm, instance.klass, name, arg_count)
}

@(private = "file")
bind_method :: proc(vm: ^VM, klass: ^ObjClass, name: ^ObjString) -> bool {
	/* Look for the method in the method table. */
	method: Value;ok: bool
	if method, ok = table_get(&klass.methods, name); !ok {
		vm_panic(vm, "Undefined property '%s'.", name.chars)
		return false
	}

	/* Create a bound method out of the method we pulled from the table and
	 * its receiver on top of the stack. */
	bound := new_bound_method(vm.gc, vm_peek(vm, 0), as_closure(method))

	vm_pop(vm) /* Receiver. */
	vm_push(vm, obj_val(bound))
	return true
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

/* 
Define the method on top of the stack by adding it to the methods table of
the class directly below it on the stack.
*/
@(private = "file")
define_method :: proc(vm: ^VM, name: ^ObjString) {
	method := vm_peek(vm, 0)
	klass := as_class(vm_peek(vm, 1))
	table_set(&klass.methods, name, method)
	vm_pop(vm) /* Method closure. */
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
	vm_push(vm, obj_val((^Obj)(result)))
}

/* Free all allocated `Obj`s. */
free_objects :: proc(gc: ^GC) {
	object := gc.objects

	for object != nil {
		next := object.next
		free_object(gc, object)
		object = next
	}
}
