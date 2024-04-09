package zen

import "core:fmt"

/* Amount of bytes to allocate before the next garbage collection. */
GC_HEAP_GROW_FACTOR :: 2

/* All values required by the GC as well as values whose memory is managed by
 * the GC (e.g. the table consisting of all the allocated strings). */
GC :: struct {
	/* All of the objects that have been allocated. */
	objects:         ^Obj,
	globals:         Table,
	bytes_allocated: int, /* Total number of bytes allocated. */
	next_gc:         int, /* Threshold for the next garbage collection. */

	/* Stack of gray objects. */
	gray_stack:      [dynamic]^Obj,
	gray_count:      int,

	/* Argument for mark_roots. */
	mark_roots_arg:  RootSource,

	/* All the strings that have been allocated. */
	strings:         Table,

	/* The "init" string. */
	init_string:     ^ObjString,

	/* The list of standard modules built into the language.
     * Except for `typeof()`, `str()`, `parse()`, `puts()` and `gets()`, all
     * built-in functions are accessed via a standard module.
     * Not defined directly once the GC starts, but only when the native modules
     * are to be defined in init_builtin_modules() */
	std_modules:     [dynamic]string,

	/* The stack of modules that have been imported so far in the program,
     * represented by their paths. For instance, if you run "a.zn" which imports
     * "b.zn" which then imports "c.zn", the import stack would be something like
     * ["a.zn", "b.zn", "c.zn"] */
	import_stack:    [dynamic]string,
}

/* Source for the roots for the root marking step in the GC. */
RootSource :: union {
	^Parser,
	^VM,
}

/* Check if the root source is the VM. */
is_vm :: #force_inline proc(source: RootSource) -> bool {
	_, ok := source.(^VM)
	return ok
}

/* Check if the root source is the parser. */
is_parser :: #force_inline proc(source: RootSource) -> bool {
	_, ok := source.(^Parser)
	return ok
}

/* Cast the `source` to a VM pointer. */
as_vm :: #force_inline proc(source: RootSource) -> ^VM {
	return source.(^VM)
}

/* Cast the `source` to a parser pointer. */
as_parser :: #force_inline proc(source: RootSource) -> ^Parser {
	return source.(^Parser)
}

/* Initialize the GC and all its values. */
init_gc :: proc() -> GC {
	return(
		GC {
			objects = nil,
			bytes_allocated = 0,
			next_gc = 1024 * 1024,
			gray_stack = make([dynamic]^Obj, 0, 0),
			gray_count = 0,
			mark_roots_arg = nil,
			strings = init_table(),
			init_string = nil,
			std_modules = make([dynamic]string),
			import_stack = make([dynamic]string),
			globals = init_table(),
		} \
	)
}

/* Free the GC's memory; also frees all allocated objects. */
free_gc :: proc(gc: ^GC) {
	free_table(&gc.strings)
	free_table(&gc.globals)
	gc.init_string = nil
	free_objects(gc)
	delete(gc.std_modules)
	delete(gc.import_stack)
	delete(gc.gray_stack)
}

/* Push a value on the stack temporarily. */
temp_push :: proc(gc: ^GC, value: Value) {
	switch s in gc.mark_roots_arg {
	case ^Parser:
		{
			/* The parser stores the previous mark_roots_arg of the GC, which is
            the VM, so we temporarily restore it. */
			vm_push(as_vm(s.prev_mark_roots), value)
		}
	case ^VM:
		vm_push(as_vm(s), value)
	}
}

/* Pop a value from the stack temporarily. */
temp_pop :: proc(gc: ^GC) -> Value {
	value: Value
	switch s in gc.mark_roots_arg {
	case ^Parser:
		{
			value = vm_pop(as_vm(s.prev_mark_roots))
			gc.mark_roots_arg = s
		}
	case ^VM:
		value = vm_pop(as_vm(s))
	}

	return value
}

/* Mark an object. */
mark_object :: proc(gc: ^GC, object: ^Obj) {
	if object == nil {return}

	/* Make sure that we're not marking an object twice. */
	if object.is_marked {return}

	if config.log_gc {
		fmt.eprintf("%p mark ", object)
		print_value(obj_val(object))
		fmt.eprintf(" of type %s\n", type_of_obj(object))
	}

	/* Set the Obj as marked. At this point, the object is gray. */
	object.is_marked = true

	/* Strings and Native functions don't have any object references to
    be traced, so they are immediately black. */
	if object.type == .STRING || object.type == .NATIVE {
		blacken_object(gc, object)
		return
	}

	/* Add the Obj to the gray stack. */
	append(&gc.gray_stack, object)
	gc.gray_count += 1
}

/* Mark a `Value`. Note that only `Obj`s need marking, since only they are
 * garbage collected. */
mark_value :: proc(gc: ^GC, value: Value) {
	/* Only Objs are garbage collected. */
	if (is_obj(value)) {
		mark_object(gc, as_obj(value))
	}
}

/* Mark all values in an array. */
mark_array :: proc(gc: ^GC, array: ^ValueArray) {
	for i in 0 ..< array.count {
		mark_value(gc, array.values[i])
	}
}

/* Mark all the keys and values in a table. */
mark_table :: proc(gc: ^GC, table: ^Table) {
	for i in 0 ..< table.capacity {
		entry := &table.entries[i]
		mark_object(gc, entry.key)
		mark_value(gc, entry.value)
	}
}

/* Mark roots for the `Compiler`; this involves marking each function the 
 * compiler is compiling into. */
mark_compiler_roots :: proc(gc: ^GC, compiler: ^Compiler) {
	cmp := compiler
	/* Mark each ObjFunction the compiler is compiling into. */
	for cmp != nil {
		mark_object(gc, (^Obj)(compiler.function))
		cmp = cmp.enclosing
	}
}

/* Mark all roots for the VM; including its stack, closures in the callframe,
 * open upvalues. */
mark_vm_roots :: proc(gc: ^GC, vm: ^VM) {
	/* Mark the VM's stack */
	for value in vm.stack {
		mark_value(gc, value)
	}

	/* Mark closure objects in the CallFrames. These need to be marked to 
    access constants and upvalues. */
	for i in 0 ..< vm.frame_count {
		mark_object(gc, (^Obj)(vm.frames[i].closure))
	}

	/* Mark the open upvalues. */
	for upvalue := vm.open_upvalues; upvalue != nil; upvalue = upvalue.next_upvalue {
		mark_object(gc, (^Obj)(upvalue))
	}
}

/* Start the root marking step of the GC. */
mark_roots :: proc(gc: ^GC, source: RootSource) {
	/* Mark global variables. */
	mark_table(gc, &gc.globals)

	switch s in source {
	case ^Parser:
		mark_compiler_roots(gc, s.current_compiler)
	case ^VM:
		mark_vm_roots(gc, s)
	}
}

/* 
This function runs just before the sweep phase, and removes every white 
object from the strings, before it is swept. This makes sure that there are 
no dangling pointers to the strings in the table (which is used more like a
hash set than a hashmap since just the keys are used).
*/
fix_weak :: proc(gc: ^GC) {
	for i in 0 ..< gc.strings.capacity {
		/* If the key is unmarked, it is white. Delete it. */
		entry := &gc.strings.entries[i]
		if entry.key != nil && !entry.key.obj.is_marked {
			table_delete(&gc.strings, entry.key)
		}
	}
}

/* Blacken a `Obj`, indicating that it is reachable. */
blacken_object :: proc(gc: ^GC, object: ^Obj) {
	if config.log_gc {
		fmt.eprintf("%p blacken ", object)
		print_value(obj_val(object))
		fmt.eprintf(" of type %s\n", type_of_obj(object))
	}

	switch object.type {
	/* A bound method contains a reference to the receiver and the
		method. */
	case .BOUND_METHOD:
		{
			bound := (^ObjBoundMethod)(object)
			mark_value(gc, bound.receiver)
			mark_object(gc, (^Obj)(bound.method))
		}
	/* A class contains a reference to an ObjString with its name and a table
	    containing its methods. */
	case .CLASS:
		{
			klass := (^ObjClass)(object)
			mark_object(gc, (^Obj)(klass.name))
			mark_table(gc, &klass.methods)
		}
	/* A closure contains a reference to the function it wraps, and
        to all the upvalues it captures. */
	case .CLOSURE:
		{
			closure := (^ObjClosure)(object)
			mark_object(gc, (^Obj)(closure.function))

			for i in 0 ..< closure.upvalue_count {
				mark_object(gc, (^Obj)(closure.upvalues[i]))
			}
		}
	/* A function contains a reference to an ObjString with the
        function's name, as well as a constant table packed with
        references to other objects. */
	case .FUNCTION:
		{
			function := (^ObjFunction)(object)
			mark_object(gc, (^Obj)(function.name))
			mark_array(gc, &function.chunk.constants)
		}
	/* An instance contains a reference to its class and references to
	 * its fields. */
	case .INSTANCE:
		{
			instance := (^ObjInstance)(object)
			mark_object(gc, (^Obj)(instance.klass))
			mark_table(gc, &instance.fields)
		}
	/* A list contains references to its elements. */
	case .LIST:
		{
			list := (^ObjList)(object)
			mark_array(gc, &list.items)
		}
	/* A module contains a reference to an ObjString with its name and
     * a table with its values. */
	case .MODULE:
		{
			module := (^ObjModule)(object)
			mark_object(gc, (^Obj)(module.name))
			mark_table(gc, &module.values)
		}
	/* An upvalue contains a reference to a closed-over value if
        it is closed. */
	case .UPVALUE:
		mark_value(gc, (^ObjUpvalue)(object).closed)
	/* Strings and native functions have no references to other
        objects. */
	case .NATIVE, .STRING:
		return
	}
}

/* Trace all references that can be reached. */
trace_references :: proc(gc: ^GC) {
	for gc.gray_count > 0 {
		object := pop(&gc.gray_stack)
		gc.gray_count -= 1
		blacken_object(gc, object)
	}
}

/* Last step of the GC; sweep everything that cannot be reached. */
sweep :: proc(gc: ^GC) {
	/* At this point, all objects in the graph are black or white. We'll
    sweep everything that's white. */
	previous: ^Obj = nil
	object := gc.objects

	/* Walk through all the objects. */
	for object != nil {
		/* If the object is black, skip it, but not before turning it white
        for the next collection. */
		if object.is_marked {
			object.is_marked = false
			previous = object
			object = object.next
		} else {
			/* Here, the object is white. Grab it, unlink it from the list
            and free it. */
			unreached := object
			object = object.next

			/* Make sure to cover the case where we're freeing the first
            node. */
			if previous != nil {
				previous.next = object
			} else {
				gc.objects = object
			}

			free_object(gc, unreached)
		}
	}
}

/* Run a GC cycle. */
collect_garbage :: proc(gc: ^GC) {
	if config.log_gc {
		fmt.eprintln("-- gc begin")
	}

	before := gc.bytes_allocated
	mark_roots(gc, gc.mark_roots_arg)
	trace_references(gc)
	fix_weak(gc)
	sweep(gc)

	gc.next_gc = gc.bytes_allocated * GC_HEAP_GROW_FACTOR

	if config.log_gc {
		fmt.eprintln("-- gc end")
		fmt.eprintf(
			"-- collected %d bytes (from %d to %d), next collection at %d\n",
			before - gc.bytes_allocated,
			before,
			gc.bytes_allocated,
			gc.next_gc,
		)
	}
}
