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
    Range,
    ^Obj,
}

type_of_value :: proc (value: Value) -> typeid {
    switch v in value {
        case bool: return bool
        case f64: return f64
        case ^Obj: return ^Obj
        case Range: return Range
        case: return nil
    }
}

is_bool :: proc (value: Value) -> bool {
    _, ok := value.(bool)
    return ok
}
is_nil :: proc (value: Value) -> bool { return value == nil }
is_number :: proc (value: Value) -> bool {
    _, ok := value.(f64)
    return ok
}
is_obj :: proc (value: Value) -> bool {
    _, ok := value.(^Obj)
    return ok
}

as_obj :: proc (value: Value) -> ^Obj { return value.(^Obj) }
as_bool :: proc (value: Value) -> bool { return value.(bool) }
as_number :: proc (value: Value) -> f64 { return value.(f64) }
as_range :: proc (value: Value) -> Range { return value.(Range) }

bool_val :: proc (value: bool) -> Value { return Value(value) }
nil_val :: proc () -> Value { return Value(nil) }
number_val :: proc (value: f64) -> Value { return Value(value) }
range_val :: proc (value: Range) -> Value { return Value(value) }
obj_val :: proc (value: ^Obj) -> Value { return Value(value) }

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
                fmt.printf("%d", int(v))
            } else {
                fmt.printf("%g", v)
            }
        case ^Obj:
            print_object(v)
        case Range:
            fmt.printf("%d%s%d", 
                v.start, v.type == .INCLUSIVE ? "..=" : "..", v.end)
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
        case f64:  return v == as_number(a)
        case ^Obj: return as_obj(a) == as_obj(b)
        case Range: return v == as_range(b)
        case: return true
    }  
}
