package zen

import "core:fmt"
import "core:math"
import "core:mem"
import "core:slice"
import "core:strings"

/* 
Whether to use NaN boxing to represent values. Set to false to use a tagged 
union representation instead.
*/
NAN_BOXING :: true

when NAN_BOXING {
	/* A u64 with only the highest bit (the sign bit) set. */
	SIGN_BIT :: cast(u64)0x8000000000000000

	/*
	A NaN value with the highest mantissa bit as well as the second highest 
	mantissa bit (the Intel QNaN bit) set.
	*/
	QNAN :: cast(u64)0x7ffc000000000000

	TAG_NIL :: 1 // 01.
	TAG_FALSE :: 2 // 10.
	TAG_TRUE :: 3 // 11.

	/* Every value can be represented by a unsigned 64-bit integer. */
	Value :: u64

	/* 
	Converting a f64 to a NaN-boxed value is as simple as transmuting it.
    For every other type, the Value is a quiet NaN, i.e. its 11 exponent bits
    are all 1 and its highest mantissa bit is also 1. That leaves us with 51
    mantissa bits and 1 sign bit. We don't use one of 51 mantissa bits which
    is a special "QNaN Floating-Point Indefinite" value, leaving us a total 
    of 51 bits we can use any way we want. We can set aside some bit 
    patterns there to represent nil, true, false et cetera, and even though 
    pointers techically need 64 bits, most widely used chips today only use 
    48 bits for pointers, so we can stuff the pointer with three bytes to 
    spare. Those remaining three bytes can be used as type tags to 
    distinguish between the types. This is what NaN boxing is all about.
	*/

	/*
	ORing FALSE_VAL (10 in binary) with 1 siply gives 11 in binary, which
    equals TRUE_VAL, and ORing TRUE_VAL gives itself.
	*/
	is_bool :: #force_inline proc(value: Value) -> bool {
		return (value | 1) == TRUE_VAL
	}
	is_nil :: #force_inline proc(value: Value) -> bool {
		return value == nil_val()
	}

	/*
	In case of a number, ANDing it with the QNAN constant must not produce
    QNAN itself since well, that's a NaN.
	*/
	is_number :: #force_inline proc(value: Value) -> bool {
		return (value & QNAN) != QNAN
	}

	/*
	A quiet NaN with its sign bit set is said to be an object pointer in
    zen's NaN boxing convention.
	*/
	is_obj :: #force_inline proc(value: Value) -> bool {
		return (value & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT)
	}

	as_number :: #force_inline proc(value: Value) -> f64 {
		return transmute(f64)value
	}
	as_bool :: #force_inline proc(value: Value) -> bool {
		return value == TRUE_VAL
	}

	/*
	This simply involves extracting the address from the last 48 bits of
    the value and casting it twice to turn it into an object pointer.
	*/
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

	/*
	If the sign bit is set in a Value and that value is a quiet NaN, it is 
    an object pointer. So, we just jam the sign bit, the qNaN bits and the
    pointer address together into the 64 bits.
	*/
	obj_val :: #force_inline proc(obj: ^Obj) -> Value {
		return (Value)(SIGN_BIT | QNAN | cast(u64)(cast(uintptr)obj))
	}
} else {
	Value :: union {
		bool,
		f64,
		^Obj,
	}

	is_bool :: #force_inline proc(value: Value) -> bool {
		_, ok := value.(bool)
		return ok
	}
	is_nil :: #force_inline proc(value: Value) -> bool {return value == nil}
	is_number :: #force_inline proc(value: Value) -> bool {
		_, ok := value.(f64)
		return ok
	}
	is_obj :: #force_inline proc(value: Value) -> bool {
		_, ok := value.(^Obj)
		return ok
	}

	as_obj :: #force_inline proc(value: Value) -> ^Obj {return value.(^Obj)}
	as_bool :: #force_inline proc(value: Value) -> bool {return value.(bool)}
	as_number :: #force_inline proc(value: Value) -> f64 {return value.(f64)}

	bool_val :: #force_inline proc(value: bool) -> Value {return Value(value)}
	nil_val :: #force_inline proc() -> Value {return Value(nil)}
	number_val :: #force_inline proc(value: f64) -> Value {return Value(value)}
	obj_val :: #force_inline proc(value: ^Obj) -> Value {return Value(value)}
}

/* 
A wrapper around a dynamic array that works as a constant pool for values. 
*/
ValueArray :: struct {
	values: [dynamic]Value,
	count:  int,
}

/* Initialize the constant pool. */
init_value_array :: proc() -> ValueArray {
	return ValueArray{values = make([dynamic]Value, 0, 0), count = 0}
}

/* Write to the constant pool. */
write_value_array :: proc(a: ^ValueArray, value: Value) {
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
free_value_array :: proc(a: ^ValueArray) {
	delete(a.values)
}

@(private = "file")
is_integer :: proc(value: f64) -> bool {
	return value == math.floor(value)
}

type_of_value :: proc(value: Value) -> string {
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
		case bool:
			return "boolean"
		case f64:
			return "number"
		case ^Obj:
			return type_of_obj(v)
		case:
			return "nil"
		}
	}

	unreachable()
}

stringify_value :: proc(value: Value) -> (res: string, was_allocation: bool) {
	when NAN_BOXING {
		if is_nil(value) {
			return "nil", false
		} else if is_bool(value) {
			return (as_bool(value) ? "true" : "false"), false
		} else if is_number(value) {
			if is_integer(as_number(value)) {
				return (fmt.tprintf("%d", int(as_number(value)))), false
			} else {
				return (fmt.tprintf("%.3f", as_number(value))), false
			}
		} else if is_obj(value) {
			str, was_allocation := stringify_object(as_obj(value))
			if was_allocation {
				return str, true
			}
			return str, false
		}
	} else {
		switch v in value {
		case bool:
			return (v ? "true" : "false"), false
		case f64:
			if is_integer(v) {
				return (fmt.tprintf("%d", int(v))), false
			} else {
				return (fmt.tprintf("%.3f", v)), false
			}
		case ^Obj:
			str, was_allocation := stringify_object(v)
			if was_allocation {
				return str, true
			}
			return str, false
		case:
			return "nil", false
		}
	}

	unreachable()
}

/* Print out `value` in a human-readable format. */
print_value :: proc(value: Value) {
	str, was_allocation := stringify_value(value)
	defer if was_allocation {
		delete(str)
	}
	fmt.print(str)
}

/* Determine if two `Value`s are equal. */
values_equal :: proc(a: Value, b: Value) -> bool {
	when NAN_BOXING {
		/* As per IEEE 754, NaN is not equal to itself. That means we'd need
         * to check if the two numbers are NaN values and return false if so.
         * That's not directly doable, but we can just compare the two values
         * as number types (if they are) and Odin will do the rest. However,
         * that is if it was possible in zen to produce NaN values like
         * infinity or such, since things that could produce NaN like division
         * by zero and adding numbers to other types are disallowed and just 
         * cause the VM to panic. Thus, there is no need to take that into
         * consideration. */
		return a == b
	} else {
		if type_of_value(a) != type_of_value(b) {
			return false
		}

		switch v in a {
		case bool:
			return v == as_bool(b)
		case f64:
			return v == as_number(b)
		case ^Obj:
			return as_obj(a) == as_obj(b)
		case:
			return true
		}
	}

	unreachable()
}
