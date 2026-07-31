package zen

import "base:intrinsics"
import "core:fmt"
import "core:strings"

/* The type of an `Obj`. */
ObjType :: enum {
	CLOSURE,
	FUNCTION,
	LIST,
	MODULE,
	NATIVE,
	RESULT,
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
NativeFn :: #type proc(vm: ^VM, arg_count: int, args: []Value) -> (value: Value, success: bool)

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

	/* Location is on the stack if the upvalue is open, otherwise the location
     * is the `closed` field of its own object. */
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

/* A list. Stores a ValueArray for its items. */
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

/* The result type. Wraps a value that may either be of a `ok` state or an
`err` state, based on the value of `is_ok`. */
ObjResult :: struct {
	using obj: Obj,
	is_ok:     bool,
	value:     Value,
}

/* Return the type of an `Obj`. */
obj_type :: #force_inline proc(value: Value) -> ObjType {
	return as_obj(value).type
}

is_closure :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .CLOSURE)
}

is_function :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .FUNCTION)
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

is_result :: #force_inline proc(value: Value) -> bool {
	return is_obj_type(value, .RESULT)
}

as_closure :: #force_inline proc(value: Value) -> ^ObjClosure {
	return (^ObjClosure)(as_obj(value))
}

as_function :: #force_inline proc(value: Value) -> ^ObjFunction {
	return (^ObjFunction)(as_obj(value))
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

as_ostring :: #force_inline proc(value: Value) -> string {
	return (^ObjString)(as_string(value)).chars
}

as_module :: #force_inline proc(value: Value) -> ^ObjModule {
	return (^ObjModule)(as_obj(value))
}

as_result :: #force_inline proc(value: Value) -> ^ObjResult {
	return (^ObjResult)(as_obj(value))
}

is_obj_type :: #force_inline proc(value: Value, type: ObjType) -> bool {
	return is_obj(value) && as_obj(value).type == type
}

type_of_obj :: proc(obj: ^Obj) -> string {
	switch obj.type {
	case .FUNCTION, .NATIVE, .CLOSURE:
		return "function"
	case .LIST:
		return "list"
	case .MODULE:
		return "module"
	case .RESULT:
		return "result"
	case .STRING:
		return "string"
	case .UPVALUE:
		return "upvalue"
	}

	fmt.panicf("invalid object type %s", obj.type)
}

allocate_obj :: proc(
	gc: ^GC,
	$T: typeid,
	type: ObjType,
) -> ^Obj where intrinsics.type_is_subtype_of(T, Obj) {
	gc.bytes_allocated += size_of(T)
	when ODIN_DEBUG {
		if opt.stress_gc {
			collect_garbage(gc)
		}
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

	when ODIN_DEBUG {
		if opt.log_gc {
			fmt.eprintf("%p allocate %d for type %v\n", obj, size_of(obj), type_of_obj(obj))
		}
	}

	return obj
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

new_result :: proc(gc: ^GC, is_ok: bool, value: Value) -> ^ObjResult {
	result := cast(^ObjResult)(allocate_obj(gc, ObjResult, .RESULT))
	result.is_ok = is_ok
	result.value = value
	return result
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

@(private = "file")
allocate_string :: proc(gc: ^GC, str: string, hash: u32) -> ^ObjString {
	zstring := as_string(obj_val(allocate_obj(gc, ObjString, .STRING)))
	zstring.chars = str
	zstring.hash = hash
	zstring.len = strings.rune_count(str)

	vm: ^VM
	/* Need to do this little dance to get the VM. */
	switch s in gc.mark_roots_arg {
	case ^VM:
		vm = s
	case ^Codegen:
		vm = as_vm(s.prev_mark_roots)
	}

	/* Stash the string on the stack so it doesn't get collected. */
	vm_push(vm, obj_val(zstring))
	table_set(&gc.strings, zstring, nil_val())
	vm_pop(vm)

	return zstring
}

hash_string :: #force_inline proc(str: string) -> u32 {
	hash: u32 = 2166136261
	for c in str {
		hash ~= u32(c)
		hash *= 16777619
	}
	return hash
}

/* Print the string representation of a function. */
stringify_function :: proc(fn: ^ObjFunction) -> string {
	if fn.name == nil {
		return fmt.tprintf("<script>")
	}

	return fmt.tprintf("<func %s>", fn.name.chars)
}

stringify_object :: proc(obj: ^Obj, quote_strings: bool = false) -> string {
	switch obj.type {
	case .CLOSURE:
		return stringify_function(as_closure(obj_val(obj)).function)
	case .FUNCTION:
		return stringify_function(as_function(obj_val(obj)))
	case .LIST:
		{
			list := as_list(obj_val(obj))
			sb := strings.builder_make()
			defer strings.builder_destroy(&sb)

			strings.write_string(&sb, "[")

			for i := 0; i < len(list.items); i += 1 {
				value := stringify_value(list.items[i], quote_strings = true)
				strings.write_string(&sb, value)

				if i != len(list.items) - 1 {
					strings.write_string(&sb, ", ")
				}
			}

			strings.write_string(&sb, "]")
			str := strings.to_string(sb)
			return fmt.tprint(str)
		}
	case .MODULE:
		return fmt.tprintf("<module %s>", as_module(obj_val(obj)).name.chars)
	case .RESULT:
		result := as_result(obj_val(obj))
		within := stringify_value(result.value, quote_strings = true)
		if result.is_ok {
			return fmt.tprintf("ok(%s)", within)
		} else {
			return fmt.tprintf("err(%s)", within)
		}
	case .NATIVE:
		return "<native func>"
	case .STRING:
		if quote_strings {
			return fmt.tprintf("\"%s\"", as_ostring(obj_val(obj)))
		} else {
			return as_ostring(obj_val(obj))
		}
	case .UPVALUE:
		return "upvalue"
	}

	fmt.eprintln("bug: reached unreachable code")
	unreachable()
}

free_object :: proc(gc: ^GC, obj: ^Obj) {
	gc.bytes_allocated -= size_of(obj)

	when ODIN_DEBUG {
		if opt.log_gc {
			str := stringify_object(obj)
			fmt.eprintf("%p free %s of type %v", obj, str, type_of_obj(obj))
		}
	}

	switch obj.type {
	case .CLOSURE:
		closure := (^ObjClosure)(obj)
		delete(closure.upvalues)
		free(closure)
	case .FUNCTION:
		fn := (^ObjFunction)(obj)
		free_chunk(&fn.chunk)
		free(fn)
	case .LIST:
		list := (^ObjList)(obj)
		free_value_array(list.items)
		free(list)
	case .MODULE:
		module := (^ObjModule)(obj)
		free_table(&module.values)
		free(module)
	case .RESULT:
		result := (^ObjResult)(obj)
		free(result)
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
