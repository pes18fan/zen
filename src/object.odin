package zen

import "core:fmt"
import "core:strings"

/* The type of an `Obj`. */
ObjType :: enum {
	BOUND_METHOD,
	CLASS,
	CLOSURE,
	FUNCTION,
	INSTANCE,
	LIST,
	MODULE,
	NATIVE,
	STRING,
	UPVALUE,
}

/* An object allocated on the heap. */
Obj :: struct {
	type:      ObjType,
	is_marked: bool,
	next:      ^Obj,
}

/* A function object. */
ObjFunction :: struct {
	using obj:     Obj,
	arity:         u8,
	upvalue_count: int,
	chunk:         Chunk,
	name:          ^ObjString,

	// Whether the function has returned in its highest scope.
	has_returned:  bool,
}

/* A function available to use in programs written into the compiler itself. */
NativeFn :: #type proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool)

/* A native function implemented in Odin itself. */
ObjNative :: struct {
	using obj: Obj,
	function:  NativeFn,
	arity:     int,
}

/* 
A string object. It has a string with its actual content, and a hash for
quick comparison, as well as its length.
*/
ObjString :: struct {
	using obj: Obj,
	chars:     string,
	len:       int,
	hash:      u32,
}

/* An upvalue. Upvalues are local variables from an enclosing function. */
ObjUpvalue :: struct {
	using obj:    Obj,
	location:     ^Value,

	/* If an upvalue is closed, it lives here. */
	closed:       Value,

	/* All upvalues are connected. This linked list allows the VM to close
	upvalues when necessary by looking through it. */
	next_upvalue: ^ObjUpvalue,
}

/*
A closure. Simply a wrapper around a function.
Naked ObjFunctions are not used during runtime, instead they are immediately
wrapped in a ObjClosure instead. This has a bit of a runtime cost but it does
make things easier.
*/
ObjClosure :: struct {
	using obj:     Obj,
	function:      ^ObjFunction,
	upvalues:      []^ObjUpvalue,
	upvalue_count: int,
}

/*
A class. Classes store a table of methods. They don't store fields since in
zen, fields can be added freely to an instance without having to be declared
in the class declaration.
*/
ObjClass :: struct {
	using obj: Obj,
	name:      ^ObjString,
	methods:   Table,
}

/*
An instance of some class. Instances store a table of their fields.
*/
ObjInstance :: struct {
	using obj:   Obj,
	klass:       ^ObjClass,
	fields:      Table,
	field_count: int,
}

/*
A bound method is a runtime object that stores a method after it is accessed
from an instance. We know that a method access and method call can be 
seperated; for example by storing the method in a variable and calling it
later on. Since the method will probably need to remember which instance it was
accessed from (the receiver), we need this new runtime type to wrap it in.
*/
ObjBoundMethod :: struct {
	using obj: Obj,
	/* Note that the receiver can only be an ObjInstance; calling it a Value
	 * here is simply for convenience and to avoid having to cast it every
	 * time. */
	receiver:  Value,
	method:    ^ObjClosure,
}

/* A list. */
ObjList :: struct {
	using obj: Obj,
	items:     ValueArray,
}

/* 
A module is not much more than a hash table containing the variables in the
global scope of an imported module.
*/
ObjModule :: struct {
	using obj: Obj,
	name:      ^ObjString,
	values:    Table,
}

/* Return the type of an `Obj`. */
obj_type :: #force_inline proc(value: Value) -> ObjType {
	return as_obj(value).type
}

is_bound_method :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .BOUND_METHOD)
}

is_class :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .CLASS)
}

is_closure :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .CLOSURE)
}

is_function :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .FUNCTION)
}

is_instance :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .INSTANCE)
}

is_list :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .LIST)
}

is_native :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .NATIVE)
}

is_string :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .STRING)
}

is_module :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .MODULE)
}

as_bound_method :: #force_inline proc(value: Value) -> ^ObjBoundMethod {
	return (^ObjBoundMethod)(as_obj(value))
}

as_class :: #force_inline proc(value: Value) -> ^ObjClass {
	return (^ObjClass)(as_obj(value))
}

as_closure :: #force_inline proc(value: Value) -> ^ObjClosure {
	return (^ObjClosure)(as_obj(value))
}

as_function :: #force_inline proc(value: Value) -> ^ObjFunction {
	return (^ObjFunction)(as_obj(value))
}

as_instance :: #force_inline proc(value: Value) -> ^ObjInstance {
	return (^ObjInstance)(as_obj(value))
}

as_list :: #force_inline proc(value: Value) -> ^ObjList {
	return (^ObjList)(as_obj(value))
}

as_native_obj :: #force_inline proc(value: Value) -> ^ObjNative {
	return (^ObjNative)(as_obj(value))
}

as_native :: #force_inline proc(value: Value) -> NativeFn {
	return (^ObjNative)(as_obj(value)).function
}

as_string :: #force_inline proc(value: Value) -> ^ObjString {
	return (^ObjString)(as_obj(value))
}

as_cstring :: #force_inline proc(value: Value) -> string {
	return (^ObjString)(as_string(value)).chars
}

as_module :: #force_inline proc(value: Value) -> ^ObjModule {
	return (^ObjModule)(as_obj(value))
}

is_obj_type :: #force_inline proc(value: Value, type: ObjType) -> bool {
	return is_obj(value) && as_obj(value).type == type
}

type_of_obj :: proc(obj: ^Obj) -> string {
	switch obj.type {
	case .CLASS:
		return "class"
	case .BOUND_METHOD, .FUNCTION, .NATIVE, .CLOSURE:
		return "function"
	case .INSTANCE:
		return "instance"
	case .LIST:
		return "list"
	case .MODULE:
		return "module"
	case .STRING:
		return "string"
	case .UPVALUE:
		return "upvalue"
	}

	unreachable()
}

allocate_obj :: proc(gc: ^GC, $T: typeid, type: ObjType) -> ^Obj {
	gc.bytes_allocated += size_of(T)
	if config.stress_gc {
		collect_garbage(gc)
	}

	/* When the total number of bytes allocated exceeds the next GC threshold,
	 * invoke a collection. */
	if gc.bytes_allocated > gc.next_gc {
		collect_garbage(gc)
	}

	obj := new(T)
	obj.type = type
	obj.is_marked = false

	obj.next = gc.objects
	gc.objects = obj

	if config.log_gc {
		fmt.eprintf("%p allocate %d for type %v\n", obj, size_of(obj), type_of_obj(obj))
	}

	return obj
}

new_bound_method :: proc(gc: ^GC, receiver: Value, method: ^ObjClosure) -> ^ObjBoundMethod {
	bound := cast(^ObjBoundMethod)(allocate_obj(gc, ObjBoundMethod, .BOUND_METHOD))
	bound.receiver = receiver
	bound.method = method
	return bound
}

new_class :: proc(gc: ^GC, name: ^ObjString) -> ^ObjClass {
	klass := cast(^ObjClass)(allocate_obj(gc, ObjClass, .CLASS))
	klass.name = name
	klass.methods = init_table()
	return klass
}

new_closure :: proc(gc: ^GC, function: ^ObjFunction) -> ^ObjClosure {
	upvalues := make([]^ObjUpvalue, function.upvalue_count) // allocate with the upvalue count of the function!!!!!!!!!

	closure := cast(^ObjClosure)(allocate_obj(gc, ObjClosure, .CLOSURE))
	closure.function = function
	closure.upvalues = upvalues
	closure.upvalue_count = function.upvalue_count
	return closure
}

new_function :: proc(gc: ^GC) -> ^ObjFunction {
	fn := cast(^ObjFunction)(allocate_obj(gc, ObjFunction, .FUNCTION))
	fn.arity = 0
	fn.upvalue_count = 0
	fn.name = nil
	fn.chunk = init_chunk()
	fn.has_returned = false
	return fn
}

new_instance :: proc(gc: ^GC, klass: ^ObjClass) -> ^ObjInstance {
	instance := cast(^ObjInstance)(allocate_obj(gc, ObjInstance, .INSTANCE))
	instance.klass = klass
	instance.fields = init_table()
	return instance
}

new_list :: proc(gc: ^GC) -> ^ObjList {
	list := cast(^ObjList)(allocate_obj(gc, ObjList, .LIST))
	list.items = init_value_array()
	return list
}

new_module :: proc(gc: ^GC, name: ^ObjString) -> ^ObjModule {
	module := cast(^ObjModule)(allocate_obj(gc, ObjModule, .MODULE))
	module.name = name
	module.values = init_table()
	return module
}

new_native :: proc(gc: ^GC, function: NativeFn, arity: int) -> ^ObjNative {
	native := cast(^ObjNative)(allocate_obj(gc, ObjNative, .NATIVE))
	native.function = function
	native.arity = arity
	return native
}

new_upvalue :: proc(gc: ^GC, slot: ^Value) -> ^ObjUpvalue {
	upvalue := cast(^ObjUpvalue)(allocate_obj(gc, ObjUpvalue, .UPVALUE))
	upvalue.closed = nil_val()
	upvalue.location = slot
	upvalue.next_upvalue = nil
	return upvalue
}

/* Return a newly allocated copy of a string, or an interned one. */
copy_string :: proc(gc: ^GC, str: string) -> ^ObjString {
	s := strings.clone(str)
	hash := hash_string(s)

	interned := table_find_string(&gc.strings, s, hash)
	if interned != nil {
		delete(s)
		return interned
	}
	return allocate_string(gc, s, hash)
}


allocate_string :: proc(gc: ^GC, str: string, hash: u32) -> ^ObjString {
	zstring := as_string(obj_val(allocate_obj(gc, ObjString, .STRING)))
	zstring.chars = str
	zstring.hash = hash
	zstring.len = len(str)

	vm: ^VM
	/* Need to do this little dance to get the VM. */
	switch s in gc.mark_roots_arg {
	case ^VM:
		vm = s
	case ^Parser:
		vm = as_vm(s.prev_mark_roots)
	}

	/* Stash the string on the stack so it doesn't get collected. */
	vm_push(vm, obj_val(zstring))
	table_set(&gc.strings, zstring, nil_val())
	vm_pop(vm)

	return zstring
}

hash_string :: proc(str: string) -> u32 {
	hash: u32 = 2166136261
	for c in str {
		hash ~= u32(c)
		hash *= 16777619
	}
	return hash
}

/* Take ownership of a string and return an interned one if it exists. */
take_string :: proc(gc: ^GC, str: string) -> ^ObjString {
	hash := hash_string(str)
	interned := table_find_string(&gc.strings, str, hash)
	if interned != nil {
		delete(str)
		return interned
	}
	return allocate_string(gc, str, hash)
}

/* Print the string representation of a function. */
stringify_function :: proc(fn: ^ObjFunction) -> string {
	if fn.name == nil {
		return fmt.tprintf("<script>")
	}

	return fmt.tprintf("<func %s>", fn.name.chars)
}

stringify_object :: proc(obj: ^Obj) -> (res: string, was_allocation: bool) {
	switch obj.type {
	case .BOUND_METHOD:
		/* Bound methods are an implementation detail, we don't expose that
			 * since from the user's perspective they're just functions. */
		return stringify_function(as_bound_method(obj_val(obj)).method.function), false
	case .CLASS:
		return fmt.tprintf("<class %s>", as_class(obj_val(obj)).name.chars), false
	case .CLOSURE:
		return stringify_function(as_closure(obj_val(obj)).function), false
	case .FUNCTION:
		return stringify_function(as_function(obj_val(obj))), false
	case .INSTANCE:
		return fmt.tprintf("%s instance", as_instance(obj_val(obj)).klass.name.chars), false
	case .LIST:
		{
			list := as_list(obj_val(obj))
			sb := strings.builder_make()
			defer strings.builder_destroy(&sb)

			strings.write_string(&sb, "[")

			for i := 0; i < list.items.count; i += 1 {
				value, v_was_allocation := stringify_value(list.items.values[i])
				defer if v_was_allocation {
					delete(value)
				}
				strings.write_string(&sb, value)

				if i != list.items.count - 1 {
					strings.write_string(&sb, ", ")
				}
			}

			strings.write_string(&sb, "]")
			str := strings.to_string(sb)
			return strings.clone(str), true
		}
	case .MODULE:
		return fmt.tprintf("%s module", as_module(obj_val(obj)).name.chars), false
	case .NATIVE:
		return "<native func>", false
	case .STRING:
		return as_cstring(obj_val(obj)), false
	case .UPVALUE:
		return "upvalue", false
	}

	unreachable()
}

free_object :: proc(gc: ^GC, obj: ^Obj) {
	gc.bytes_allocated -= size_of(obj)

	if config.log_gc {
		fmt.eprintf("%p free ", obj)
		str, was_allocation := stringify_object(obj)
		defer if was_allocation {
			delete(str)
		}
		fmt.eprintf(str)
		fmt.eprintf(" of type %v\n", type_of_obj(obj))
	}

	switch obj.type {
	case .BOUND_METHOD:
		/* Only free the bound method itself, not its references; to ensure
	     * that `this` can still find that object if the method gets stored
		 * and invoked later. */
		bound := (^ObjBoundMethod)(obj)
		free(bound)
	case .CLASS:
		klass := (^ObjClass)(obj)
		free_table(&klass.methods)
		free(klass)
	case .CLOSURE:
		closure := (^ObjClosure)(obj)
		delete(closure.upvalues)
		free(closure)
	case .FUNCTION:
		fn := (^ObjFunction)(obj)
		free_chunk(&fn.chunk)
		free(fn)
	case .INSTANCE:
		instance := (^ObjInstance)(obj)
		free_table(&instance.fields)
		free(instance)
	case .LIST:
		list := (^ObjList)(obj)
		free_value_array(&list.items)
		free(list)
	case .MODULE:
		module := (^ObjModule)(obj)
		free_table(&module.values)
		free(module)
	case .NATIVE:
		fn := (^ObjNative)(obj)
		free(fn)
	case .STRING:
		zstr := (^ObjString)(obj)
		delete(zstr.chars)
		free(zstr)
	case .UPVALUE:
		upvalue := (^ObjUpvalue)(obj)
		free(upvalue)
	}
}
