package zen

import "core:fmt"
import "core:strings"

ObjType :: enum {
    STRING,
}

/* An object allocated on the heap. */
Obj :: struct {
    type: ObjType,
    next: ^Obj,
}

ObjString :: struct {
    using obj: Obj,
    chars: string,
    hash: u32,
}

obj_type :: #force_inline proc (value: Value) -> ObjType { 
    return as_obj(value).type 
}

is_string :: #force_inline proc (value: Value) -> bool {
    return is_obj_type(value, .STRING)
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

allocate_obj :: proc (vm: ^VM, $T: typeid, type: ObjType) -> ^Obj {
    obj := new(T)
    obj.type = type
    obj.next = vm.objects
    vm.objects = obj
    return obj
}

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

print_object :: proc (obj: ^Obj) {
    switch obj.type {
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
        case .STRING:
            zstr := as_string(obj)
            delete(zstr.chars)
            free(zstr)
    }
}
