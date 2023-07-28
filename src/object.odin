package zen

import "core:fmt"
import "core:strings"

ObjType :: enum {
	CLOSURE,
	FUNCTION,
	NATIVE,
	STRING,
	UPVALUE,
}

/* An object allocated on the heap. */
Obj :: struct {
	type: ObjType,
	next: ^Obj,
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

obj_type :: #force_inline proc(value: Value) -> ObjType {
	return as_obj(value).type
}

is_closure :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .CLOSURE)
}

is_function :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .FUNCTION)
}

is_native :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .NATIVE)
}

is_string :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .STRING)
}

as_closure :: #force_inline proc(value: Value) -> ^ObjClosure {
	return (^ObjClosure)(as_obj(value))
}

as_function :: #force_inline proc(value: Value) -> ^ObjFunction {
	return (^ObjFunction)(as_obj(value))
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

is_obj_type :: #force_inline proc(value: Value, type: ObjType) -> bool {
	return is_obj(value) && as_obj(value).type == type
}

type_of_obj :: proc(obj: ^Obj) -> string {
	switch obj.type {
	case .FUNCTION, .NATIVE, .CLOSURE:
		return "function"
	case .STRING:
		return "string"
	case .UPVALUE:
		return "upvalue"
	}

	unreachable()
}

allocate_obj :: proc(v: ^VM, $T: typeid, type: ObjType) -> ^Obj {
	obj := new(T)
	obj.type = type
	obj.next = v.objects
	v.objects = obj
	return obj
}

new_closure :: proc(vm: ^VM, function: ^ObjFunction) -> ^ObjClosure {
	upvalues := make([]^ObjUpvalue, function.upvalue_count) // allocate with the upvalue count of the function!!!!!!!!!

	closure := cast(^ObjClosure)(allocate_obj(vm, ObjClosure, .CLOSURE))
	closure.function = function
	closure.upvalues = upvalues
	closure.upvalue_count = function.upvalue_count
	return closure
}

new_function :: proc(vm: ^VM) -> ^ObjFunction {
	fn := cast(^ObjFunction)(allocate_obj(vm, ObjFunction, .FUNCTION))
	fn.arity = 0
	fn.upvalue_count = 0
	fn.name = nil
	fn.chunk = init_chunk()
	fn.has_returned = false
	return fn
}

new_native :: proc(vm: ^VM, function: NativeFn, arity: int) -> ^ObjNative {
	native := cast(^ObjNative)(allocate_obj(vm, ObjNative, .NATIVE))
	native.function = function
	native.arity = arity
	return native
}

new_upvalue :: proc(vm: ^VM, slot: ^Value) -> ^ObjUpvalue {
	upvalue := cast(^ObjUpvalue)(allocate_obj(vm, ObjUpvalue, .UPVALUE))
	upvalue.closed = nil_val()
	upvalue.location = slot
	upvalue.next_upvalue = nil
	return upvalue
}

/* Return a newly allocated copy of a string, or an interned one. */
copy_string :: proc(vm: ^VM, str: string) -> ^ObjString {
	s := strings.clone(str)
	hash := hash_string(s)

	interned := table_find_string(&vm.strings, s, hash)
	if interned != nil {
		delete(s)
		return interned
	}
	return allocate_string(vm, s, hash)
}

/* Print the string representation of a function. */
print_function :: proc(fn: ^ObjFunction) {
	if fn.name == nil {
		fmt.printf("<script>")
		return
	}

	fmt.printf("<func %s>", fn.name.chars)
}

/* Print the string representation of an object. */
print_object :: proc(obj: ^Obj) {
	switch obj.type {
	case .CLOSURE:
		print_function(as_closure(obj).function)
	case .FUNCTION:
		print_function(as_function(obj))
	case .NATIVE:
		fmt.printf("<native func>")
	case .STRING:
		fmt.printf("%s", as_cstring(obj))
	case .UPVALUE:
		fmt.printf("upvalue")
	}
}

allocate_string :: proc(v: ^VM, str: string, hash: u32) -> ^ObjString {
	zstring := as_string(allocate_obj(v, ObjString, .STRING))
	zstring.chars = str
	zstring.hash = hash
	zstring.len = len(str)
	table_set(&v.strings, zstring, nil_val())
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
take_string :: proc(v: ^VM, str: string) -> ^ObjString {
	hash := hash_string(str)
	interned := table_find_string(&v.strings, str, hash)
	if interned != nil {
		delete(str)
		return interned
	}
	return allocate_string(v, str, hash)
}

free_object :: proc(obj: ^Obj) {
	switch obj.type {
	case .CLOSURE:
		closure := (^ObjClosure)(obj)
		delete(closure.upvalues)
		free(closure)
	case .FUNCTION:
		fn := (^ObjFunction)(obj)
		free_chunk(&fn.chunk)
		free(fn)
	case .NATIVE:
		fn := (^ObjNative)(obj)
		free(fn)
	case .STRING:
		zstr := as_string(obj)
		delete(zstr.chars)
		free(zstr)
	case .UPVALUE:
		upvalue := (^ObjUpvalue)(obj)
		free(upvalue)
	}
}
