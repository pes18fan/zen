package zen

import "base:intrinsics"
import "core:fmt"
import "core:math"
import "core:mem"
import vmem "core:mem/virtual"
import "core:os"
import "core:slice"
import "core:strings"
import "core:time"

FRAMES_MAX :: 96

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
     * cyclic imports. Value is the file path for a file and "REPL" for a REPL
     * since you can't import REPLs. */
	path:             string,

	/* Just the path, except with the file extension and other path stuff stripped
     * out, like in a module name. For instance, for a path "a/b/c.zn", the name
     * will be just "c". Used for working with user-defined modules, but NOT
     * for distinguishing them. For a REPL, this string is "REPL". */
	name:             string,

	/* The chunk being interpreted. */
	chunk:            ^Chunk,

	/* The stack of values. */
	stack:            [STACK_MAX]Value,
	stack_top:        int,

	/* Call frames present in the chunk. */
	frames:           [FRAMES_MAX]CallFrame,
	frame_count:      int,

	/* Table of compile-time global variables; necessary for the REPL. */
	compiler_globals: Table,

	/* Linked list of all open upvalues. */
	open_upvalues:    ^ObjUpvalue,

	/* Pointer to the GC. */
	gc:               ^GC,

	/* Arguments passed to the program. */
	args:             ^ObjList,

	/* "Registers" of the VM. */
	// TODO: make `it` a stack to allow nested pipelines
	it:               Value, // stores the intermediate value of a pipeline
	save:             Value, // general purpose, currently stores return value of a block
}

/* The result of the interpreting. */
InterpretResult :: enum {
	INTERPRET_OK,
	INTERPRET_VOLUNTARY_EXIT,
	INTERPRET_LEX_ERROR,
	INTERPRET_PARSE_ERROR,
	INTERPRET_COMPILE_ERROR,
	INTERPRET_READ_ERROR,
	INTERPRET_RUNTIME_ERROR,
}

/* Raise a runtime error. */
vm_panic :: proc(vm: ^VM, format: string, args: ..any) {
	fmt.eprint(color_red("panic: "))
	fmt.eprintfln("%s", fmt.tprintf(format, ..args))

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

	fmt.eprint(color_yellow("  (at"))
	if vm.path == "REPL" {
		fmt.eprint(color_yellow(" REPL)\n"))
	} else {
		fmt.eprint(color_yellow(fmt.tprintf(" file %s)\n", vm.path)))
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
	vm_push(vm, obj_val(obj_module)) // keep it on the stack so that gc doesn't collect

	module_functions := get_builtin_module(gc, module)

	for function in module_functions {
		vm_push(vm, obj_val(copy_string(gc, function.name)))
		vm_push(vm, obj_val(new_native(gc, function.function, function.arity)))

		table_set(&obj_module.values, as_string(vm_peek(vm, 1)), vm_peek(vm, 0))

		vm_pop(vm)
		vm_pop(vm)
	}

	// module stays on the stack
}

/* Resets the stack. */
reset_stack :: proc(vm: ^VM) {
	vm.stack_top = -1
	vm.frame_count = 0
	vm.open_upvalues = nil
}

/* Returns a newly created VM. */
init_VM :: proc() -> VM {
	vm := VM {
		name             = "",
		path             = "",
		chunk            = nil,
		open_upvalues    = nil,
		compiler_globals = init_table(),
		stack_top        = -1,
		frame_count      = 0,
		it               = nil_val(),
		save             = nil_val(),
	}

	return vm
}

/* Free's the VM's memory. */
free_VM :: proc(vm: ^VM) {
	free_table(&vm.compiler_globals)

	// don't free explicitly, let gc do it
	vm.it = nil_val()
	vm.save = nil_val()
}

/* Reads a byte from the chunk and increments the instruction pointer. */
read_byte :: #force_inline proc(frame: ^CallFrame) -> byte #no_bounds_check {
	defer frame.ip = mem.ptr_offset(frame.ip, 1)
	return frame.ip^
}

read_short :: #force_inline proc(frame: ^CallFrame) -> int #no_bounds_check {
	defer frame.ip = mem.ptr_offset(frame.ip, 2)
	return int(frame.ip^) << 8 | int(mem.ptr_offset(frame.ip, 1)^)
}

/* Reads a constant from the chunk and pushes it onto the stack. */
read_constant :: #force_inline proc(frame: ^CallFrame) -> Value #no_bounds_check {
	return frame.closure.function.chunk.constants.values[read_byte(frame)]
}

read_constant_long :: #force_inline proc(frame: ^CallFrame) -> Value #no_bounds_check {
	return frame.closure.function.chunk.constants.values[read_short(frame)]
}

read_string :: #force_inline proc(frame: ^CallFrame) -> ^ObjString {
	return as_string(read_constant(frame))
}

read_string_long :: #force_inline proc(frame: ^CallFrame) -> ^ObjString {
	return as_string(read_constant_long(frame))
}

/*
Performs a binary operation on the top two values of the stack. In zen, except
for the concatenation operator '..' which is handled separately, a binary
operator can only return either a 64-bit float or a boolean. All of these
operators take numbers as their arguments, hence 'numeric'.
*/
@(private = "file")
numeric_binary_op :: #force_inline proc(
	vm: ^VM,
	$returns: typeid,
	op: string,
) -> InterpretResult where intrinsics.type_is_numeric(returns) ||
	intrinsics.type_is_boolean(returns) {
	if !is_number(vm_peek(vm, 0)) || !is_number(vm_peek(vm, 1)) {
		vm_panic(
			vm,
			"Expected numbers as operands to '%s', got %v and %v instead.",
			op,
			type_of_value(vm_peek(vm, 1)),
			type_of_value(vm_peek(vm, 0)),
		)
		return .INTERPRET_RUNTIME_ERROR
	}

	b := as_number(vm_pop(vm))
	a := as_number(vm_pop(vm))

	switch typeid_of(returns) {
	case f64:
		switch op {
		case "+":
			vm_push(vm, number_val(a + b))
		case "-":
			vm_push(vm, number_val(a - b))
		case "*":
			vm_push(vm, number_val(a * b))
		case "/":
			if b == 0 {
				vm_panic(vm, "Cannot divide by zero.")
				return .INTERPRET_RUNTIME_ERROR
			}
			vm_push(vm, number_val(a / b))
		case "%":
			if b == 0 {
				vm_panic(vm, "Cannot modulo by zero.")
				return .INTERPRET_RUNTIME_ERROR
			}
			vm_push(vm, number_val(math.mod(a, b)))
		case:
			fmt.panicf("Invalid numeric binary operation '%s'.\n", op)
		}
	case bool:
		switch op {
		case ">":
			vm_push(vm, bool_val(a > b))
		case "<":
			vm_push(vm, bool_val(a < b))
		}
	case:
		fmt.panicf("Invalid return type for binary operation '%s'.\n", op)
	}

	return nil
}

@(private = "file")
print_stack :: proc(vm: ^VM) {
	fmt.printf("          ")
	for i := 0; i <= vm.stack_top; i += 1 {
		value := vm.stack[i]
		fmt.eprintf("[ ")
		print_value(os.stderr, value)
		fmt.eprintf(" ]")
	}
	fmt.eprintf("\n")
}

/*
Run the VM, going through the bytecode and interpreting each instruction
one by one.
*/
@(private = "file")
run :: proc(vm: ^VM, importer: Maybe(ImportingModule) = nil) -> InterpretResult #no_bounds_check {
	frame := &vm.frames[vm.frame_count - 1]

	for {
		when ODIN_DEBUG {
			if config.trace_exec {
				print_stack(vm)
				offset := mem.ptr_sub(frame.ip, &frame.closure.function.chunk.code[0])
				disassemble_instruction(&frame.closure.function.chunk, offset)
			}
		}

		instruction := OpCode(read_byte(frame))

		switch instruction {
		case .OP_CONSTANT:
			constant := read_constant(frame)
			vm_push(vm, constant)
		case .OP_CONSTANT_LONG:
			constant := read_constant_long(frame)
			vm_push(vm, constant)
		case .OP_NIL:
			vm_push(vm, nil_val())
		case .OP_TRUE:
			vm_push(vm, bool_val(true))
		case .OP_FALSE:
			vm_push(vm, bool_val(false))
		case .OP_POP:
			vm_pop(vm)
		case .OP_POPN:
			n := read_byte(frame)
			for _ in 0 ..< n {
				vm_pop(vm)
			}
		case .OP_DUP:
			vm_push(vm, vm_peek(vm, 0))
		case .OP_GET_LOCAL:
			slot := read_byte(frame)
			vm_push(vm, mem.ptr_offset(frame.slots, slot)^)
		case .OP_SET_LOCAL:
			slot := read_byte(frame)
			mem.ptr_offset(frame.slots, slot)^ = vm_peek(vm, 0)
		case .OP_GET_GLOBAL:
			name := read_string(frame)

			/* No runtime check is done for variable existence since that is
             * done at compile time. */
			value, _ := table_get(&vm.gc.globals, name)
			vm_push(vm, value)
		case .OP_GET_GLOBAL_LONG:
			name := read_string_long(frame)
			value, _ := table_get(&vm.gc.globals, name)
			vm_push(vm, value)
		case .OP_DEFINE_GLOBAL:
			name := read_string(frame)
			table_set(&vm.gc.globals, name, vm_peek(vm, 0))
			vm_pop(vm)
		case .OP_DEFINE_GLOBAL_LONG:
			name := read_string_long(frame)
			table_set(&vm.gc.globals, name, vm_peek(vm, 0))
			vm_pop(vm)
		case .OP_SET_GLOBAL:
			name := read_string(frame)

			/* No runtime check is done for variable existence since that is
             * done at compile time. */
			table_set(&vm.gc.globals, name, vm_peek(vm, 0))
		case .OP_SET_GLOBAL_LONG:
			name := read_string_long(frame)
			table_set(&vm.gc.globals, name, vm_peek(vm, 0))
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
		// This opcode is used to get values in a module.
		case .OP_GET_PROPERTY:
			{
				if is_module(vm_peek(vm, 0)) {
					module := as_module(vm_peek(vm, 0))
					name := read_string(frame)

					/* Look for the value in the module. */
					value: Value; ok: bool
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
				} else {
					vm_panic(vm, "Only modules allow dot access.")
					return .INTERPRET_RUNTIME_ERROR
				}
			}
		case .OP_GET_PROPERTY_LONG:
			{
				if is_module(vm_peek(vm, 0)) {
					module := as_module(vm_peek(vm, 0))
					name := read_string_long(frame)

					value: Value; ok: bool
					if value, ok = table_get(&module.values, name); ok {
						vm_pop(vm)
						vm_push(vm, value)
						break
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
				} else {
					vm_panic(vm, "Only modules allow dot access.")
					return .INTERPRET_RUNTIME_ERROR
				}
			}
		case .OP_GET_IT:
			vm_push(vm, vm.it)
		case .OP_SET_IT:
			vm.it = vm_pop(vm)
		case .OP_GET_SAVE:
			vm_push(vm, vm.save)
		case .OP_SET_SAVE:
			vm.save = vm_pop(vm)
		case .OP_EQUAL:
			b := vm_pop(vm)
			a := vm_pop(vm)
			vm_push(vm, bool_val(values_equal(a, b)))
		case .OP_GREATER:
			numeric_binary_op(vm, bool, ">") or_return
		case .OP_LESS:
			numeric_binary_op(vm, bool, "<") or_return
		case .OP_ADD:
			numeric_binary_op(vm, f64, "+") or_return
		case .OP_SUBTRACT:
			numeric_binary_op(vm, f64, "-") or_return
		case .OP_MULTIPLY:
			numeric_binary_op(vm, f64, "*") or_return
		case .OP_DIVIDE:
			numeric_binary_op(vm, f64, "/") or_return
		case .OP_MODULO:
			numeric_binary_op(vm, f64, "%") or_return
		case .OP_CONCAT:
			if !is_string(vm_peek(vm, 0)) || !is_string(vm_peek(vm, 1)) {
				vm_panic(
					vm,
					"Expected strings as operands to '..', got %v and %v instead.",
					type_of_value(vm_peek(vm, 1)),
					type_of_value(vm_peek(vm, 0)),
				)
				return .INTERPRET_RUNTIME_ERROR
			}
			concatenate(vm)
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
			// leave the value on the stack
			print_value(os.stdout, vm_peek(vm, 0), quote_strings = true)
			fmt.println()
		case .OP_PRINT_REPL:
			fmt.print("=> ")
			print_value(os.stdout, vm_peek(vm, 0), quote_strings = true)
			fmt.println()
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
		case .OP_JUMP_IF_TRUE:
			{
				offset := read_short(frame)
				if !is_falsey(vm_peek(vm, 0)) {
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
		case .OP_INVOKE_LONG:
			{
				method := read_string_long(frame)
				arg_count := read_byte(frame)

				if !invoke(vm, method, int(arg_count)) {
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
					vm_panic(vm, "List index must be a number.")
					return .INTERPRET_RUNTIME_ERROR
				}

				index := as_number(b)

				if !is_integer(index) || index < 0 {
					vm_panic(vm, "List index must be a non-negative integer.")
					return .INTERPRET_RUNTIME_ERROR
				}

				list := as_list(a)

				if int(index) >= list.items.count {
					vm_panic(
						vm,
						fmt.tprintf(
							"Index out of bounds, attempted indexing %d in a size %d list.",
							int(index),
							list.items.count,
						),
					)
					return .INTERPRET_RUNTIME_ERROR
				}

				vm_push(vm, list.items.values[int(index)])
			}
		case .OP_SUBSCRIPT_SET:
			{
				c := vm_pop(vm)
				b := vm_pop(vm)
				a := vm_pop(vm)

				if !is_list(a) {
					vm_panic(vm, "Can only set elements of a list.")
					return .INTERPRET_RUNTIME_ERROR
				}

				if !is_number(b) {
					vm_panic(vm, "List index must be a positive integer.")
					return .INTERPRET_RUNTIME_ERROR
				}

				index := as_number(b)

				if math.floor(index) != index || index < 0 {
					vm_panic(vm, "List index must be a non-negative integer.")
					return .INTERPRET_RUNTIME_ERROR
				}

				list := as_list(a)

				if int(index) >= list.items.count {
					vm_panic(
						vm,
						fmt.tprintf(
							"Index out of bounds, attempted indexing %d in a %d list.",
							int(index),
							list.items.count,
						),
					)
					return .INTERPRET_RUNTIME_ERROR
				}

				// Update the list and push it back
				list.items.values[int(index)] = c
				vm_push(vm, obj_val(list))
			}
		case .OP_CLOSURE:
			{
				// the function is always a long constant in a closure
				function := as_function(read_constant_long(frame))

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
				importing_module, imported := importer.?
				if is_public && imported {
					table_set(
						&importing_module.module.values,
						closure.function.name,
						vm_peek(vm, 0),
					)
				}
			}
		case .OP_CLOSE_UPVALUE:
			close_upvalues(vm, &vm.stack[vm.stack_top])
			vm_pop(vm)
		case .OP_CLOSE_LOOP_VAR:
			slot := read_byte(frame)
			close_upvalues(vm, mem.ptr_offset(frame.slots, int(slot))) // don't pop here
		case .OP_RETURN:
			result := vm_pop(vm) // Retrieve the return value from the stack.

			// Close any upvalues that were captured inside the returning function.
			close_upvalues(vm, frame.slots)
			vm.frame_count -= 1

			if vm.frame_count == 0 {
				vm_pop(vm)
				return .INTERPRET_OK
			}

			/* Pop off all of the local variables and arguments of the
			   function. */
			for i := vm.stack_top; i >= 0; i -= 1 {
				value := vm.stack[i]
				if values_equal(value, frame.slots^) {
					break
				}
				vm_pop(vm)
			}
			assert(vm.stack_top >= 0, "stack must not become empty before program end") // There should be at least the function left here
			vm_pop(vm) // Pop the function itself.

			vm_push(vm, result) // Push the return value back to the stack.
			frame = &vm.frames[vm.frame_count - 1]
		case .OP_MODULE_BUILTIN:
			module_str := read_string(frame).chars

			module, ok := as_builtin_module(module_str)
			if !ok {
				fmt.panicf("unknown builtin module '%v", module_str)
			}

			define_builtin_module(vm.gc, module_str, module)
		case .OP_MODULE_BUILTIN_LONG:
			module_str := read_string_long(frame).chars

			module, ok := as_builtin_module(module_str)
			if !ok {
				fmt.panicf("unknown builtin module '%v", module_str)
			}

			define_builtin_module(vm.gc, module_str, module)
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
					importer = ImportingModule{path = vm.path, name = vm.name, module = module},
				)
				if result != .INTERPRET_OK {
					return result /* Return the errored program back out */
				}

				pop(&vm.gc.import_stack) /* Remove the path from the import stack. */

				vm_push(vm, obj_val(module))
			}
		case .OP_MODULE_USER_LONG:
			{
				module_name := read_string_long(frame)
				module_path := read_string_long(frame)

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
					importer = ImportingModule{path = vm.path, name = vm.name, module = module},
				)
				if result != .INTERPRET_OK {
					return result /* Return the errored program back out */
				}

				pop(&vm.gc.import_stack) /* Remove the path from the import stack. */

				vm_push(vm, obj_val(module))
			}
		case .OP_ITERATE:
			{
				iterable := vm_pop(vm)

				assert(is_number(vm_peek(vm, 0)), "iteration index should be a number\n")
				idx := as_number(vm_pop(vm))

				if is_list(iterable) {
					list := as_list(iterable)

					if int(idx) < len(list.items.values) {
						vm_push(vm, list.items.values[int(idx)])
						vm_push(vm, number_val(idx + 1))
						vm_push(vm, bool_val(true))
					} else {
						vm_push(vm, bool_val(false))
					}
				} else {
					vm_panic(vm, "Can only iterate over lists, not %v.", type_of_value(iterable))
					return .INTERPRET_RUNTIME_ERROR
				}
			}
		case .OP_EXIT:
			{
				top := vm_pop(vm) /* Grab the exit code */

				if !is_number(top) {
					vm_panic(vm, "Exit code must be a number, not %v.", type_of_value(top))
					return .INTERPRET_RUNTIME_ERROR
				}

				code := as_number(top)
				if code < 0 {
					vm_panic(vm, "Exit code must be non-negative.")
					return .INTERPRET_RUNTIME_ERROR
				}

				// clear the stack
				reset_stack(vm)

				zen_update_exit_code(uint(code))
				return .INTERPRET_VOLUNTARY_EXIT
			}
		}
	}
}

/* Interpret a chunk. */
interpret :: proc(
	vm: ^VM,
	gc: ^GC,
	source: string,
	importer: Maybe(ImportingModule) = nil,
	persistent_globals: ^map[string]^UntypedVariable = nil,
	persistent_typechecker: ^TypeChecker = nil,
) -> InterpretResult {
	/* If the name of the VM and the importing module are both the same (if the
     * importing module is not nil), then we have a cyclic import, which causes
     * all sorts of problems. So we have to disallow that. */
	_, importer_exists := importer.?
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

	arena: vmem.Arena
	err := vmem.arena_init_growing(&arena)
	ensure(err == nil)
	defer vmem.arena_destroy(&arena)

	arena_allocator := vmem.arena_allocator(&arena)
	prev_alloc := context.allocator
	context.allocator = arena_allocator

	source_in_arena := strings.clone(source) // clone into the arena
	tokens, lx_ok := lex(source_in_arena)
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

	when ODIN_DEBUG {
		if config.dump_tokens {
			fmt.println("TOKENS:")
			for token in tokens {
				fmt.printf("    %v", token.type)
				if token.type != .EOF {
					fmt.printf("(%s)", token.lexeme)
				}
				fmt.printfln(" at line %d, column %d", token.position.line, token.position.column)
			}

			return .INTERPRET_OK
		}
	}

	expr, ps_ok := parse(tokens)
	if !ps_ok {
		return .INTERPRET_PARSE_ERROR
	}

	/* Time the parser. */
	if config.record_time {
		time.stopwatch_stop(&sw)
		fmt.eprintf("Parser: %v\n", time.stopwatch_duration(sw))
		time.stopwatch_reset(&sw)
		time.stopwatch_start(&sw)
	}

	when ODIN_DEBUG {
		if config.dump_ast {
			str := ast_string(expr)
			fmt.println(str)

			return .INTERPRET_OK
		}
	}

	sm_ok := semcheck(expr)
	if !sm_ok {
		return .INTERPRET_COMPILE_ERROR
	}

	/* Time semantic analysis. */
	if config.record_time {
		time.stopwatch_stop(&sw)
		fmt.eprintf("Semantic analyzer: %v\n", time.stopwatch_duration(sw))
		time.stopwatch_reset(&sw)
		time.stopwatch_start(&sw)
	}

	_, mod_ok := create_module_graph(zen_get_path(), source, tokens, expr)
	if !mod_ok {
		return .INTERPRET_COMPILE_ERROR
	}

	/* Time module resolution. */
	if config.record_time {
		time.stopwatch_stop(&sw)
		fmt.eprintf("Module resolver: %v\n", time.stopwatch_duration(sw))
		time.stopwatch_reset(&sw)
		time.stopwatch_start(&sw)
	}

	reso, rs_ok := resolve_full(vm, expr, persistent_globals)
	if !rs_ok {
		return .INTERPRET_COMPILE_ERROR
	}

	/* Time the resolver. */
	if config.record_time {
		time.stopwatch_stop(&sw)
		fmt.eprintf("Resolver: %v\n", time.stopwatch_duration(sw))
		time.stopwatch_reset(&sw)
		time.stopwatch_start(&sw)
	}

	when TYPE_CHECK {
		should_not_typecheck := has_user_modules(expr)
	}

	TYPE_CHECK :: true
	// TODO: type checker pass, in progress
	when TYPE_CHECK {
		if !should_not_typecheck {
			_, tc_ok := typecheck_full(vm, expr, reso, persistent_typechecker)

			if !tc_ok {
				return .INTERPRET_COMPILE_ERROR
			}

			/* Time the typechecker. */
			if config.record_time {
				time.stopwatch_stop(&sw)
				fmt.eprintf("Typechecker: %v\n", time.stopwatch_duration(sw))
				time.stopwatch_reset(&sw)
				time.stopwatch_start(&sw)
			}
		}
	}

	// TODO: optimization pass: constant folding, inlining whenever possible,
	// remove instructions that cancel each other out (e.g. in push -> pop -> push,
	// keep just the last push), remove instructions that make no difference
	// to the final (correct) result

	context.allocator = prev_alloc
	collect_globals(&vm.compiler_globals, gc, expr)
	fn, cg_ok := codegen(gc, expr, &vm.compiler_globals)
	if !cg_ok {
		return .INTERPRET_COMPILE_ERROR
	}

	/* Time the compiler. */
	if config.record_time {
		time.stopwatch_stop(&sw)
		fmt.eprintf("Compiler: %v\n", time.stopwatch_duration(sw))
		time.stopwatch_reset(&sw)
		time.stopwatch_start(&sw)
	}

	// empty program
	if fn == nil {
		return .INTERPRET_OK
	}

	/* Time the VM. */
	defer if config.record_time {
		time.stopwatch_stop(&sw)
		fmt.eprintf("\nVM: %v\n", time.stopwatch_duration(sw))
	}

	/* If the user only wants to compile the script, then we can stop here. */
	when ODIN_DEBUG {
		if config.compile_only {
			return .INTERPRET_OK
		}
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
	vm.stack_top += 1
	vm.stack[vm.stack_top] = value
}

/* Pop a value out of the stack. */
vm_pop :: #force_inline proc(vm: ^VM) -> Value #no_bounds_check {
	assert(vm.stack_top >= 0, "vm stack must not be empty\n")
	defer vm.stack_top -= 1
	return vm.stack[vm.stack_top]
}

/* Peek at a certain distance from the top of the stack. */
vm_peek :: #force_inline proc(vm: ^VM, distance: int) -> Value #no_bounds_check {
	return vm.stack[vm.stack_top - distance]
}

/* Returns true if provided value is falsey. */
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

	// Subtract the stack top index by the number of args to get the beginning
	// of the frame.
	frame.slots = &vm.stack[vm.stack_top - arg_count]
	return true
}

/* Call a value if its a callable, else panic. */
@(private = "file")
call_value :: proc(vm: ^VM, callee: Value, arg_count: int) -> (success: bool) {
	if is_obj(callee) {
		#partial switch obj_type(callee) {
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

			// Add a 1 to the stack indexing to exclude the native function itself
			result, ok := native(vm, arg_count, vm.stack[vm.stack_top - arg_count + 1:])
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

	vm_panic(vm, "Can only call functions.")
	return false
}

/* Invoke a method or a function in a module. */
@(private = "file")
invoke :: proc(vm: ^VM, name: ^ObjString, arg_count: int) -> bool {
	receiver := vm_peek(vm, arg_count)

	if is_module(receiver) {
		module := as_module(receiver)

		value: Value; ok: bool

		if value, ok = table_get(&module.values, name); ok {
			args := make([dynamic]Value)
			defer delete(args)

			for _ in 0 ..< int(arg_count) {
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
				"Value '%s' does not exist on module '%s'.",
				name.chars,
				module.name.chars,
			)
			vm_panic(vm, panic_str)
			return false
		}
	}

	vm_panic(vm, "Cannot invoke on a non-module value.")
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
	i = copy(chars, a.chars)
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
