package zen

import "core:math"
import "core:time"
import "core:os"
import "core:strconv"
import "core:strings"

// All the native functions in zen are in this file.

init_natives :: proc(gc: ^GC) {
	// Define native functions
	// time
	define_native(gc, "clock", clock_native, arity = 0)

	// numbers and math
	define_native(gc, "sqrt", sqrt_native, arity = 1)
	define_native(gc, "ln", ln_native, arity = 1)
	define_native(gc, "pow", pow_native, arity = 2)
	define_native(gc, "parse", parse_native, arity = 1)

	// errors
	define_native(gc, "panic", panic_native, arity = 1)

	// io
	define_native(gc, "gets", gets_native, arity = 0)

	// strings
	define_native(gc, "chomp", chomp_native, arity = 1)
	define_native(gc, "len", len_native, arity = 1)
	define_native(gc, "replace", replace_native, arity = 3)
	define_native(gc, "upcase", upcase_native, arity = 1)
	define_native(gc, "downcase", downcase_native, arity = 1)
	define_native(gc, "reverse", reverse_native, arity = 1)
}

/* Get the current UNIX time in seconds. */
clock_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return number_val(f64(time.to_unix_nanoseconds(time.now())) / 1e9), true
}

/* Read a line from stdin. */
gets_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	buf: [1024]byte
	n, err := os.read(os.stdin, buf[:])
	if err < 0 {
		vm_panic(vm, "Failed to read input.")
		return nil, false
	}

	return obj_val(copy_string(vm.gc, string(buf[:n]))), true
}

/* Panic the VM with a custom message. */
panic_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Panic message must be a string, not a %v.", type_of_value(args[0]))
		return nil, false
	}

	vm_panic(vm, "%s", as_cstring(args[0]))
	return nil, false
}

/* 
Convert a string to a number.
Panics if a non-string is passed, or if the string is malformed.
*/
parse_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Can only parse strings, not %vs.", type_of_value(args[0]))
		return nil, false
	}

	n, ok := strconv.parse_f64(as_string(args[0]).chars)
	if !ok {
		vm_panic(vm, "Cannot parse '%s' to a number.", as_string(args[0]).chars)
		return nil, false
	}

	return number_val(n), true
}

/* Find the square root of a number. */
sqrt_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(vm, "Cannot find the square root of a %v.", type_of_value(args[0]))
		return nil, false
	}

	n := as_number(args[0])

	if n < 0 {
		vm_panic(vm, "Cannot find the square root of a negative number.")
		return nil, false
	}

	return number_val(math.sqrt(n)), true
}

/* Find the natural logarithm of a number. */
ln_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(vm, "Cannot find the natural log of a %v.", type_of_value(args[0]))
	}

	n := as_number(args[0])

	if n <= 0 {
		vm_panic(vm, "Cannot find the natural log of a non-positive number.")
		return nil, false
	}

	return number_val(math.ln(n)), true
}

/* Raise a number to a power. */
pow_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) || !is_number(args[1]) {
		vm_panic(vm, "Cannot raise a %v to a power.", type_of_value(args[0]))
		return nil, false
	}

	return number_val(math.pow(as_number(args[0]), as_number(args[1]))), true
}

/* Trim whitespace from both sides of a string. */
chomp_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Cannot chomp a %v.", type_of_value(args[0]))
		return nil, false
	}

	return obj_val(copy_string(vm.gc, strings.trim_space(as_string(args[0]).chars))), true
}

/* Get the length of a string. */
len_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Cannot get length of a %v.", type_of_value(args[0]))
		return nil, false
	}

	return number_val(f64(as_string(args[0]).len)), true
}

/* 
Substitute all instances of a substring in a string with another substring.
The first argument is the string to search in, the second is the substring to 
replace, and the third is the substring to replace it with.
*/
replace_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) || !is_string(args[1]) || !is_string(args[2]) {
		vm_panic(
			vm,
			"All arguments for gsub must be strings, got %v instead.",
			type_of_value(args[0]),
		)
		return nil, false
	}

	str, was_allocation := strings.replace_all(
		as_string(args[0]).chars,
		as_string(args[1]).chars,
		as_string(args[2]).chars,
	)
	// If an extra allocation was done, free the string
	defer if was_allocation {
		delete(str)
	}

	return obj_val(copy_string(vm.gc, str)), true
}

/* Turn all the characters of a string into uppercase. */
upcase_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "upcase() requires a string, not a %v.", type_of_value(args[0]))
		return nil, false
	}

	str, err := strings.to_upper(as_string(args[0]).chars)
	if err != nil {
		vm_panic(vm, "Allocator error on upcase(): %s", err)
		return nil, false
	}

	return obj_val(take_string(vm.gc, str)), true
}

/* Turn all the characters of a string into lowercase. */
downcase_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "downcase() requires a string, not a %v.", type_of_value(args[0]))
		return nil, false
	}

	str, err := strings.to_lower(as_string(args[0]).chars)
	if err != nil {
		vm_panic(vm, "Allocator error on downcase(): %s", err)
		return nil, false
	}

	return obj_val(take_string(vm.gc, str)), true
}

/* Reverse a string. */
reverse_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Can only reverse strings, not %vs.", type_of_value(args[0]))
		return nil, false
	}

	str, err := strings.reverse(as_cstring(args[0]))
	if err != nil {
		vm_panic(vm, "Allocator error on reverse(): %s", err)
		return nil, false
	}

	return obj_val(take_string(vm.gc, str)), true
}
