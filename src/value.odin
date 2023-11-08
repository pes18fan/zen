package zen

import "core:mem"
import "core:math"
import "core:strings"
import "core:slice"
import "core:fmt"

/* Whether to use NaN boxing to represent values. Set to false to use a tagged
 * union representation instead. */
NAN_BOXING :: true

when NAN_BOXING {
    SIGN_BIT :: cast(u64)0x8000000000000000

    /* A NaN value with the highest mantissa bit as well as the second highest
     * mantissa bit (the Intel QNaN bit) set. */
    QNAN :: cast(u64)0x7ffc000000000000

    TAG_NIL   :: 1 // 01.
    TAG_FALSE :: 2 // 10.
    TAG_TRUE  :: 3 // 11.

    /* Every value can be represented by a unsigned 64-bit integer. */
    Value :: u64

    /* Converting a f64 to a NaN-boxed value is as simple as transmuting it.
     * For every other type, the Value is a quiet NaN, i.e. its 11 exponent bits
     * are all 1 and its highest mantissa bit is also 1. That leaves us with 51
     * mantissa bits and 1 sign bit. We don't use one of 51 mantissa bits which
     * is a special "QNaN Floating-Point Indefinite" value, leaving us a total 
     * of 51 bits we can use any way we want. We can set aside some bit 
     * patterns there to represent nil, true, false et cetera, and even though 
     * pointers techically need 64 bits, most widely used chips today only use 
     * 48 bits for pointers, so we can stuff the pointer with three bytes to 
     * spare. Those remaining three bytes can be used as type tags to 
     * distinguish between the types. */

    is_bool :: #force_inline proc(value: Value) -> bool {
        return (value | 1) == TRUE_VAL
    }
    is_nil :: #force_inline proc(value: Value) -> bool {
        return value == nil_val() 
    }
    is_number :: #force_inline proc(value: Value) -> bool {
        return (value & QNAN) != QNAN
    }
    is_obj :: #force_inline proc(value: Value) -> bool {
        return (value & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT)
    }

    as_number :: #force_inline proc(value: Value) -> f64 {
        return transmute(f64)value
    }
    as_bool :: #force_inline proc(value: Value) -> bool {
        return value == TRUE_VAL
    }
    as_obj :: #force_inline proc(value: Value) -> ^Obj {
        return cast(^Obj)(cast(uintptr)(value &~ (SIGN_BIT | QNAN)))
    }

    bool_val :: #force_inline proc(value: bool) -> Value {
        return value ? TRUE_VAL : FALSE_VAL
    }

    FALSE_VAL :: cast(Value)(cast(u64)(QNAN | TAG_FALSE))
    TRUE_VAL :: cast(Value)(cast(u64)(QNAN | TAG_TRUE))

    nil_val :: #force_inline proc() -> Value {
        return cast(Value)(cast(u64)(QNAN | TAG_NIL))
    }
    number_val :: #force_inline proc(num: f64) -> Value { 
        return transmute(Value)num 
    }

    /* If the sign bit is set in a Value and that value is a quiet NaN, it is 
     * an object pointer. So, we just jam the sign bit, the qNaN bits and the
     * pointer address together into the 64 bits. */
    obj_val :: #force_inline proc(obj: ^Obj) -> Value {
        return (Value)(SIGN_BIT | QNAN | cast(u64)(cast(uintptr)obj))
    }
} else {
    Value :: union {
        bool,
        f64,
        ^Obj,
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
}

/* 
A wrapper around a dynamic array that works as a constant pool for values. 
*/
ValueArray :: struct {
    values: [dynamic]Value,
    count: int,
}

/* Initialize the constant pool. */
init_value_array :: proc () -> ValueArray {
    return ValueArray {
        values = make([dynamic]Value, 0, 0),
        count = 0,
    }
}

/* Write to the constant pool. */
write_value_array :: proc (a: ^ValueArray, value: Value) {
    append(&a.values, value)
    a.count += 1
}

/* Pop a value off the constant pool. */
pop_value_array :: proc(a: ^ValueArray) -> Value {
    assert(a.count > 0)
    defer a.count -= 1
    return pop(&a.values)
}

/* Free the constant pool's memory. */
free_value_array :: proc (a: ^ValueArray) {
    delete(a.values)
}

@(private="file")
is_integer :: proc (value: f64) -> bool {
    return value == math.floor(value)
}

type_of_value :: proc (value: Value) -> string {
    when NAN_BOXING {
        if is_nil(value) {
            return "nil"
        } else if is_bool(value) {
            return "boolean"
        } else if is_number(value) {
            return "number"
        } else if is_obj(value) {
            return type_of_obj(as_obj(value))
        }
    } else {
        switch v in value {
            case bool: return "boolean"
            case f64: return "number"
            case ^Obj: return type_of_obj(v)
            case: return "nil"
        }
    }

    unreachable()
}

stringify_value :: proc (value: Value) -> string {
    when NAN_BOXING {
        if is_nil(value) {
            return "nil"
        } else if is_bool(value) {
            return as_bool(value) ? "true" : "false"
        } else if is_number(value) {
            if is_integer(as_number(value)) {
                return fmt.tprintf("%d", int(as_number(value)))
            } else {
                return fmt.tprintf("%g", as_number(value))
            }
        } else if is_obj(value) {
            return stringify_object(as_obj(value))
        }
    } else {
        switch v in value {
            case bool:
                return v ? "true" : "false"
            case f64:
                if is_integer(v) {
                    return fmt.tprintf("%d", int(v))
                } else {
                    return fmt.tprintf("%g", v)
                }
            case ^Obj:
                return stringify_object(v)
            case:
                return "nil"
        }
    }

    unreachable()
}

/* Print out `value` in a human-readable format. */
print_value :: proc (value: Value) {
    fmt.print(stringify_value(value))
}

/* Determine if two `Value`s are equal. */
values_equal :: proc (a: Value, b: Value) -> bool {
    when NAN_BOXING {
        /* We could just do `return a == b`, but if we do that the result of
         * NaN == NaN will be `true`, which is not correct as per IEEE 754.
         * From what I know, that is because there is no reason to believe that
         * two NaN's are the same non-number value. */
        if is_number(a) && is_number(b) {
            return as_number(a) == as_number(b)
        }
        return a == b
    } else {
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

    unreachable()
}
