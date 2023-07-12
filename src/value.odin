package zen

import "core:mem"
import "core:math"
import "core:strings"
import "core:slice"
import "core:fmt"

/*
Representation of a zen value in Odin. May be one of the values listed
in the union, or nil.
*/
Value :: union {
    bool,
    f64,
    ^Obj,
}

type_of_value :: proc (value: Value) -> string {
    switch v in value {
        case bool: return "boolean"
        case f64: return "number"
        case ^Obj: return type_of_obj(v)
        case: return "nil"
    }
}

is_bool :: #force_inline proc (value: Value) -> bool {
    _, ok := value.(bool)
    return ok
}
is_nil :: #force_inline proc (value: Value) -> bool { return value == nil }
is_number :: #force_inline proc (value: Value) -> bool {
    _, ok := value.(f64)
    return ok
}
is_obj :: #force_inline proc (value: Value) -> bool {
    _, ok := value.(^Obj)
    return ok
}

as_obj :: #force_inline proc (value: Value) -> ^Obj { return value.(^Obj) }
as_bool :: #force_inline proc (value: Value) -> bool { return value.(bool) }
as_number :: #force_inline proc (value: Value) -> f64 { return value.(f64) }

bool_val :: #force_inline proc (value: bool) -> Value { return Value(value) }
nil_val :: #force_inline proc () -> Value { return Value(nil) }
number_val :: #force_inline proc (value: f64) -> Value { return Value(value) }
obj_val :: #force_inline proc (value: ^Obj) -> Value { return Value(value) }

/* 
A wrapper around a dynamic array that works as a constant pool for values. 
*/
ValueArray :: struct {
    values: [dynamic]Value,
}

/* Initialize the constant pool. */
init_value_array :: proc () -> ValueArray {
    return ValueArray {
        values = make([dynamic]Value, 0, 0),
    }
}

/* Write to the constant pool. */
write_value_array :: proc (a: ^ValueArray, value: Value) {
    append(&a.values, value)
}

/* Free the constant pool's memory. */
free_value_array :: proc (a: ^ValueArray) {
    delete(a.values)
}

@(private="file")
is_integer :: proc (value: f64) -> bool {
    return value == math.floor(value)
}

/* Print out `value` in a human-readable format. */
print_value :: proc (value: Value) {
    switch v in value {
        case bool:
            fmt.print(v ? "true" : "false")
        case f64:
            if is_integer(v) {
                assert(v == math.floor(v))
                fmt.printf("%d", int(v))
            } else {
                fmt.printf("%g", v)
            }
        case ^Obj:
            print_object(v)
        case: fmt.print("nil")
    }
}

/* Determine if two `Value`s are equal. */
values_equal :: proc (a: Value, b: Value) -> bool {
    if type_of_value(a) != type_of_value(b) {
        return false
    }

    switch v in a {
        case bool: return v == as_bool(b)
        case f64:  return v == as_number(b)
        case ^Obj: return as_obj(a) == as_obj(b)
        case: return true
    }  
}
