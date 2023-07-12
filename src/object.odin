package zen

import "core:fmt"
import "core:strings"

ObjType :: enum {
    FUNCTION,
    NATIVE,
    STRING,
}

/* An object allocated on the heap. */
Obj :: struct {
    type: ObjType,
    next: ^Obj,
}

/* A function object. */
ObjFunction :: struct {
    using obj: Obj,
    arity: u8,
    chunk: Chunk,
    name: ^ObjString,
    has_returned: bool,
}

NativeFn :: #type proc(arg_count: int, args: []Value) -> Value

/* A native function implemented in Odin itself. */
ObjNative :: struct {
    using obj: Obj,
    function: NativeFn,
}

/* 
A string object. It has a string with its actual content, and a hash for
quick comparison. 
*/
ObjString :: struct {
    using obj: Obj,
    chars: string,
    hash: u32,
}

obj_type :: #force_inline proc (value: Value) -> ObjType { 
    return as_obj(value).type 
}

is_function :: #force_inline proc (value: Value) -> bool {
    return is_obj_type(value, .FUNCTION)
}

is_native :: #force_inline proc (value: Value) -> bool {
    return is_obj_type(value, .NATIVE)
}

is_string :: #force_inline proc (value: Value) -> bool {
    return is_obj_type(value, .STRING)
}

as_function :: #force_inline proc (value: Value) -> ^ObjFunction {
    return (^ObjFunction)(as_obj(value))
}

as_native :: #force_inline proc (value: Value) -> NativeFn {
    return (^ObjNative)(as_obj(value)).function
}

as_string :: #force_inline proc (value: Value) -> ^ObjString {
    return (^ObjString)(as_obj(value))
}

as_cstring :: #force_inline proc (value: Value) -> string {
    return (^ObjString)(as_string(value)).chars
}

is_obj_type :: #force_inline proc (value: Value, type: ObjType) -> bool {
    return is_obj(value) && as_obj(value).type == type
}

type_of_obj :: proc (obj: ^Obj) -> string {
    switch obj.type {
        case .FUNCTION, .NATIVE: return "function"
        case .STRING:            return "string"
    }

    unreachable()
}

allocate_obj :: proc (v: ^VM, $T: typeid, type: ObjType) -> ^Obj {
    obj := new(T)
    obj.type = type
    obj.next = v.objects
    v.objects = obj
    return obj
}

new_function :: proc (v: ^VM) -> ^ObjFunction {
    fn := cast(^ObjFunction)(allocate_obj(v, ObjFunction, .FUNCTION))
    fn.arity = 0
    fn.name = nil
    fn.chunk = init_chunk()
    fn.has_returned = false
    return fn
}

new_native :: proc (v: ^VM, function: NativeFn) -> ^ObjNative {
    native := cast(^ObjNative)(allocate_obj(v, ObjNative, .NATIVE))
    native.function = function
    return native
}

/* Return a newly allocated copy of a string, or an interned one. */
copy_string :: proc (v: ^VM, str: string) -> ^ObjString {
    s := strings.clone(str)
    hash := hash_string(s)

    interned := table_find_string(&v.strings, s, hash)
    if interned != nil {
        delete(s)
        return interned
    }
    return allocate_string(v, s, hash)
}

print_function :: proc (fn: ^ObjFunction) {
    if fn.name == nil {
        fmt.printf("<script>")
        return
    }

    fmt.printf("<func %s>", fn.name.chars)
}

/* Print the string representation of an object. */
print_object :: proc (obj: ^Obj) {
    switch obj.type {
        case .FUNCTION:
            print_function(as_function(obj))
        case .NATIVE:
            fmt.printf("<native func>")
        case .STRING:
            fmt.printf("%s", as_cstring(obj))
    }
}

allocate_string :: proc (v: ^VM, str: string, hash: u32) -> ^ObjString {
    zstring := as_string(allocate_obj(v, ObjString, .STRING))
    zstring.chars = str
    zstring.hash = hash
    table_set(&v.strings, zstring, nil_val())
    return zstring
}

hash_string :: proc (str: string) -> u32 {
    hash: u32 = 2166136261
    for c in str {
        hash ~= u32(c)
        hash *= 16777619
    }
    return hash
}

/* Take ownership of a string and return an interned one if it exists. */
take_string :: proc (v: ^VM, str: string) -> ^ObjString {
    hash := hash_string(str)
    interned := table_find_string(&v.strings, str, hash)
    if interned != nil {
        delete(str)
        return interned
    }
    return allocate_string(v, str, hash)
}

free_object :: proc (obj: ^Obj) {
    switch obj.type {
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
    }
}
