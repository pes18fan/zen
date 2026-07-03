package zen

import "core:fmt"
import "core:math"
import "core:math/rand"
import "core:os"
import "core:path/filepath"
import "core:reflect"
import "core:strconv"
import "core:strings"
import "core:time"
import "core:unicode/utf8"

BuiltinModule :: enum {
	TIME,
	MATH,
	OS,
	STRING,
	LIST,
	RESULT,
}

BuiltinFunction :: struct {
	name:     string,
	function: NativeFn,
	arity:    int,
}

as_builtin_module :: proc(name: string) -> (BuiltinModule, bool) {
	upper := strings.to_upper(name)
	defer delete(upper)

	m, ok := reflect.enum_from_name(BuiltinModule, upper)
	if !ok {
		return nil, false
	}
	return m, true
}

@(rodata)
STD_MODULE_FUNCTIONS: [BuiltinModule][]BuiltinFunction = {
	.TIME   = {{"clock", clock_native, 0}, {"clock_ms", clock_ms_native, 0}},
	.MATH   = {
		{"sin", sin_native, 1},
		{"cos", cos_native, 1},
		{"tan", tan_native, 1},
		{"sqrt", sqrt_native, 1},
		{"ln", ln_native, 1},
		{"pow", pow_native, 2},
		{"floor", floor_native, 1},
		{"ceil", ceil_native, 1},
		{"round", round_native, 1},
		{"abs", abs_native, 1},
		{"rand", rand_native, 0},
	},
	.OS     = {{"read", read_native, 1}, {"write", write_native, 3}, {"args", args_native, 0}},
	.STRING = {
		{"chomp", chomp_native, 1},
		{"replace", replace_native, 3},
		{"slice", slice_native, 3},
		{"index", index_native, 2},
		{"chars", chars_native, 1},
		{"upcase", upcase_native, 1},
		{"downcase", downcase_native, 1},
		{"reverse", reverse_native, 1},
		{"asciichar", asciichar_native, 1},
		{"asciinum", asciinum_native, 1},
		{"byte_count", byte_count_native, 1},
	},
	.LIST   = {
		{"push", push_native, 2},
		{"pop", pop_native, 1},
		{"remove_last", remove_last_native, 1},
		{"sort", sort_native, 1},
		{"sum", sum_native, 1},
	},
	.RESULT = {
		{"ok?", is_ok_native, 1},
		{"err?", is_err_native, 1},
		{"unwrap", unwrap_native, 1},
		{"unwrap_or", unwrap_or_native, 2},
	},
}

/* Get all the information required to import a builtin module into the global
 * scope. */
get_builtin_module :: #force_inline proc(gc: ^GC, module: BuiltinModule) -> []BuiltinFunction {
	return STD_MODULE_FUNCTIONS[module]
}

/* These are the functions available in the global scope. The rest are in their
corresponding modules. */
GlobalBuiltinFunction :: enum {
	PUTS,
	GETS,
	PANIC,
	ASSERT,
	LEN,
	STR,
	PARSE,
	TYPEOF,
	COPY,
	DIRNAME,
	FILENAME,
	OK,
	ERR,
}

as_global_builtin_function :: proc(name: string) -> (GlobalBuiltinFunction, bool) {
	upper := strings.to_upper(name)
	defer delete(upper)

	m, ok := reflect.enum_from_name(GlobalBuiltinFunction, upper)
	if !ok {
		return nil, false
	}
	return m, true
}

@(rodata)
GLOBAL_BUILTIN_FUNCTIONS: [GlobalBuiltinFunction]BuiltinFunction = {
	.PUTS     = {"puts", puts_native, 1},
	.GETS     = {"gets", gets_native, 0},
	.PANIC    = {"panic", panic_native, 1},
	.ASSERT   = {"assert", assert_native, 1},
	.LEN      = {"len", len_native, 1},
	.STR      = {"str", str_native, 1},
	.PARSE    = {"parse", parse_native, 1},
	.TYPEOF   = {"typeof", typeof_native, 1},
	.COPY     = {"copy", copy_native, 1},
	.DIRNAME  = {"dirname", dirname_native, 0},
	.FILENAME = {"filename", filename_native, 0},
	.OK       = {"ok", ok_native, 1},
	.ERR      = {"err", err_native, 1},
}

init_natives :: proc(gc: ^GC) {
	for fn in GLOBAL_BUILTIN_FUNCTIONS {
		define_native(gc, fn.name, fn.function, fn.arity)
	}
}


/* Print a line of text to stdout followed by a newline. */
puts_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	print_value(args[0])
	fmt.print("\n")
	return nil_val(), true
}

/* Get the length of a string or a list. */
len_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) && !is_list(args[0]) {
		vm_panic(vm, "Cannot get length of a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	if is_string(args[0]) {
		return number_val(f64(as_string(args[0]).len)), true
	} else {
		return number_val(f64(as_list(args[0]).items.count)), true
	}
}


/* Read a line from stdin. */
gets_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	buf: [1024]byte
	n, err := os.read(os.stdin, buf[:])
	if err != nil {
		vm_panic(vm, "Failed to read input: %s", os.error_string(err))
		return nil_val(), false
	}

	return obj_val(copy_string(vm.gc, string(buf[:n]))), true
}


/* Convert any value to a string. */
str_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return obj_val(copy_string(vm.gc, stringify_value(args[0]))), true
}

/* Return the type of any value, represented as a string. */
typeof_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return obj_val(copy_string(vm.gc, type_of_value(args[0]))), true
}

/* Make a deep copy of an object. */
copy_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return copy_value(vm.gc, args[0]), true
}

/* Return the name of the running program. Returns an empty string if in a REPL. */
filename_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return obj_val(copy_string(vm.gc, config.__path)), true
}

/* Return the directory containing the running program. Returns an empty string
 * if in a REPL. */
dirname_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return obj_val(copy_string(vm.gc, config.__dirname)), true
}

/* Return the `ok` variant of a result that wraps the argument. */
ok_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	result := new_result(vm.gc, is_ok = true, value = args[0])
	return obj_val(result), true
}

/* Return the `err` variant of a result that wraps the argument. */
err_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	result := new_result(vm.gc, is_ok = false, value = args[0])
	return obj_val(result), true
}

/* ---------- TIME ---------- */

/* Get the current UNIX time in seconds. */
clock_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return number_val(f64(time.to_unix_seconds(time.now()))), true
}

/* Get the current UNIX time in milliseconds. */
clock_ms_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return number_val(f64(time.to_unix_nanoseconds(time.now()) / 1e6)), true
}

/* ---------- OS ---------- */

/* Panic the VM with a custom message. */
panic_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Panic message must be a string, not a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	vm_panic(vm, "%s", as_ostring(args[0]))
	return nil_val(), false
}

/* Panic if the provided value is falsey. */
assert_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if is_falsey(args[0]) {
		vm_panic(vm, "Runtime assertion failed.")
		return nil_val(), false
	}

	return nil_val(), true
}

/* Read a file and return its data. */
read_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "The given path %v must be a string.", type_of_value(args[0]))
		return nil_val(), false
	}

	path := as_string(args[0]).chars
	abs_path, err := filepath.join([]string{config.__dirname, path}, context.allocator)
	if err != nil {
		vm_panic(vm, "Failed to get filepath for read operation: %s", os.error_string(err))
		return nil_val(), false
	}
	defer delete(abs_path)

	data, rerr := os.read_entire_file(abs_path, context.allocator)
	defer delete(data)
	if rerr != nil {
		vm_panic(vm, "Failed to read file '%s': %s", abs_path, os.error_string(rerr))
		return nil_val(), false
	}

	return obj_val(copy_string(vm.gc, string(data[:]))), true
}

/* Write to a file, either by overwriting it or appending to it. */
write_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) || !is_string(args[1]) || !is_string(args[2]) {
		vm_panic(
			vm,
			"All arguments to 'write' must be strings, not %v, %v and %v.",
			type_of_value(args[0]),
			type_of_value(args[1]),
			type_of_value(args[2]),
		)
		return nil_val(), false
	}

	path := as_string(args[0]).chars
	abs_path, err := filepath.join([]string{config.__dirname, path}, context.allocator)
	if err != nil {
		vm_panic(vm, "Failed to get filepath for write operation: %s", os.error_string(err))
		return nil_val(), false
	}

	defer delete(abs_path)

	mode := as_string(args[1]).chars
	data := as_string(args[2]).chars

	if mode == "w" {
		err := os.write_entire_file(abs_path, transmute([]u8)data)
		if err != nil {
			vm_panic(vm, "Failed to write to file '%s': %s.", path, os.error_string(err))
			return nil_val(), false
		}

		return nil_val(), true
	} else if mode == "a" {
		/* Without the S_IRUSR and S_IWUSR, the user won't be able to read or
         * write to the file at all. */
		flags: os.File_Flags
		flags = os.File_Flags{.Read, .Write, .Create, .Append}

		f, oerr := os.open(abs_path, flags)
		if oerr != nil {
			vm_panic(
				vm,
				"Failed to open file '%s' for appending: %s.",
				path,
				os.error_string(oerr),
			)
			return nil_val(), false
		}
		defer os.close(f)

		_, werr := os.write(f, transmute([]u8)data)
		if werr != nil {
			vm_panic(vm, "Failed to write to file '%s': %s.", path, os.error_string(werr))
			return nil_val(), false
		}

		return nil_val(), true
	} else {
		vm_panic(vm, "Invalid write mode '%s'.", mode)
		return nil_val(), false
	}
}

/* Get the arguments passed to the program. */
args_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return obj_val(vm.args), true
}

/* 
Convert a string to a number.
Panics if a non-string is passed, or if the string is malformed.
*/
parse_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Can only parse strings, not %vs.", type_of_value(args[0]))
		return nil_val(), false
	}

	n, ok := strconv.parse_f64(as_ostring(args[0]))
	if !ok {
		vm_panic(vm, "Cannot parse '%s' to a real number.", as_string(args[0]).chars)
		return nil_val(), false
	}

	return number_val(n), true
}

/* ---------- MATH ---------- */

/* Find the sine of a number. */
sin_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(vm, "Cannot find the sine of a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	n := as_number(args[0])
	return number_val(math.sin(n)), true
}

/* Find the cosine of a number. */
cos_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(vm, "Cannot find the cosine of a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	n := as_number(args[0])
	return number_val(math.cos(n)), true
}

/* Find the tangent of a number. */
tan_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(vm, "Cannot find the tangent of a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	n := as_number(args[0])
	return number_val(math.tan(n)), true
}

/* Find the square root of a number. */
sqrt_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(vm, "Cannot find the square root of a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	n := as_number(args[0])

	if n < 0 {
		vm_panic(vm, "Cannot use 'sqrt' to find the square root of a negative number.")
		return nil_val(), false
	}

	return number_val(math.sqrt(n)), true
}

/* Find the natural logarithm of a number. */
ln_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(
			vm,
			"Argument for 'ln' must be a positive real number, not %v.",
			type_of_value(args[0]),
		)
		return nil_val(), false
	}

	n := as_number(args[0])

	if n <= 0 {
		vm_panic(vm, "Cannot find the natural log of a non-positive number.")
		return nil_val(), false
	}

	return number_val(math.ln(n)), true
}

/* Raise a number to a power. */
pow_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) || !is_number(args[1]) {
		vm_panic(vm, "Arguments to 'pow' must be real numbers, not %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	return number_val(math.pow(as_number(args[0]), as_number(args[1]))), true
}

/* Find the largest integer smaller than a number. */
floor_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(vm, "Cannot floor a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	return number_val(math.floor(as_number(args[0]))), true
}

/* Find the smallest integer greater than a number. */
ceil_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(vm, "Cannot ceil a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	return number_val(math.ceil(as_number(args[0]))), true
}

/* Round a number to the nearest integer. */
round_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(vm, "Cannot round a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	return number_val(math.round(as_number(args[0]))), true
}

/* Find the absolute value of a real number. */
abs_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(vm, "Cannot get the absolute value of a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	return number_val(math.abs(as_number(args[0]))), true
}

/* Generate a random double floating point value in the interval [0, 1). */
rand_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return number_val(rand.float64()), true
}

/* ---------- STRING ---------- */

/* 
Substitute all instances of a substring in a string with another substring.
The first argument is the string to search in, the second is the substring to 
replace, and the third is the substring to replace it with.
*/
replace_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) || !is_string(args[1]) || !is_string(args[2]) {
		vm_panic(
			vm,
			"All arguments for replace() must be strings, got %v, %v and %v instead.",
			type_of_value(args[0]),
			type_of_value(args[1]),
			type_of_value(args[2]),
		)
		return nil_val(), false
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

/* Get a substring from a string, in the range [start,end). */
slice_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(
			vm,
			"First argument to slice() must be a string, got %v instead.",
			type_of_value(args[0]),
		)
		return nil_val(), false
	}

	if !is_number(args[1]) || !is_number(args[2]) {
		vm_panic(
			vm,
			"Second and third arguments to slice() must be numbers, got %v and %v instead.",
			type_of_value(args[1]),
			type_of_value(args[2]),
		)
		return nil_val(), false
	}

	start := as_number(args[1])
	end := as_number(args[2])
	if !is_integer(start) || !is_integer(end) {
		vm_panic(vm, "Second and third arguments to slice() must both be non-negative integers.")
		return nil_val(), false
	}

	sub, ok := strings.substring(as_string(args[0]).chars, int(start), int(end))
	if !ok {
		vm_panic(vm, "Index out of bounds in slice().")
		return nil_val(), false
	}

	return obj_val(copy_string(vm.gc, sub)), true
}

// Provided an index `k` and a string `s`, get the `k`th UTF-8 codepoint in `s`.
index_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(
			vm,
			"First argument to index() must be a string, got %v instead.",
			type_of_value(args[0]),
		)
		return nil_val(), false
	}

	if !is_number(args[1]) {
		vm_panic(
			vm,
			"Second argument to index() must be a numbers, got %v instead.",
			type_of_value(args[1]),
		)
		return nil_val(), false
	}

	index := as_number(args[1])
	if !is_integer(index) || index < 0 {
		vm_panic(vm, "Second argument to index() be a non-negative integer.")
		return nil_val(), false
	}

	runes := utf8.string_to_runes(as_string(args[0]).chars)
	defer delete(runes)

	if int(index) >= len(runes) {
		vm_panic(
			vm,
			"Index out of bounds in index(), tried indexing %v in a length %v string.",
			int(index),
			len(runes),
		)
		return nil_val(), false
	}
	char := runes[int(index)]
	res := utf8.runes_to_string([]rune{char})
	return obj_val(take_string(vm.gc, res)), true
}

// Turn a string into a list of its constituent UTF-8 codepoints.
chars_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(
			vm,
			"Argument to chars() must be a string, got %v instead.",
			type_of_value(args[0]),
		)
		return nil_val(), false
	}

	list := new_list(vm.gc)

	for r in as_string(args[0]).chars {
		res := utf8.runes_to_string([]rune{r})
		str := take_string(vm.gc, res)
		vm_push(vm, obj_val(str))
		write_value_array(&list.items, vm_pop(vm))
	}
	return obj_val(list), true
}


/* Trim whitespace from both sides of a string. */
chomp_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Cannot chomp a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	chomped := strings.trim_space(as_string(args[0]).chars)
	return obj_val(copy_string(vm.gc, chomped)), true
}

/* Turn all the characters of a string into uppercase. */
upcase_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "upcase() requires a string, not a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	str, err := strings.to_upper(as_string(args[0]).chars)
	if err != nil {
		vm_panic(vm, "Failed to run upcase(): %s", os.error_string(err))
		return nil_val(), false
	}

	return obj_val(take_string(vm.gc, str)), true
}

/* Turn all the characters of a string into lowercase. */
downcase_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "downcase() requires a string, not a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	str, err := strings.to_lower(as_string(args[0]).chars)
	if err != nil {
		vm_panic(vm, "Failed to run downcase(): %s", os.error_string(err))
		return nil_val(), false
	}

	return obj_val(take_string(vm.gc, str)), true
}

/* Reverse a string. */
reverse_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Can only reverse strings, not %vs.", type_of_value(args[0]))
		return nil_val(), false
	}

	str, err := strings.reverse(as_ostring(args[0]))
	if err != nil {
		vm_panic(vm, "Failed to run reverse(): %s", os.error_string(err))
		return nil_val(), false
	}

	return obj_val(take_string(vm.gc, str)), true
}

/* Get the ASCII character out of a number. */
asciichar_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_number(args[0]) {
		vm_panic(vm, "Cannot turn a %v into an ASCII character.", type_of_value(args[0]))
		return nil_val(), false
	}

	rn := cast(rune)(as_number(args[0]))
	str := fmt.tprintf("%c", rn)
	return obj_val(copy_string(vm.gc, str)), true
}

/* Get the ASCII number out of a character. */
asciinum_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Cannot turn a %v into an ASCII character.", type_of_value(args[0]))
		return nil_val(), false
	}

	rn := as_string(args[0]).chars[0]
	num := cast(f64)(cast(i32)(rn))
	return number_val(num), true
}

/* Get the number of bytes in a string. */
byte_count_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Cannot get the byte count of a '%v'.", type_of_value(args[0]))
		return nil_val(), false
	}

	return number_val(cast(f64)len(as_string(args[0]).chars)), true
}


/* ---------- LIST ---------- */

/* Push a value to a list. */
push_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_list(args[0]) {
		vm_panic(vm, "Cannot push a value to a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	list := as_list(args[0])
	item := args[1]

	write_value_array(&list.items, item)
	return args[0], true
}

/* Pop a value off a list and return the list. */
remove_last_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_list(args[0]) {
		vm_panic(vm, "Cannot pop a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	list := as_list(args[0])

	if list.items.count == 0 {
		vm_panic(vm, "Cannot pop an empty list.")
		return nil_val(), false
	}

	pop_value_array(&list.items)
	return args[0], true
}

/* Pop a value off a list and return that value. */
pop_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_list(args[0]) {
		vm_panic(vm, "Cannot pop a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	list := as_list(args[0])

	if list.items.count == 0 {
		vm_panic(vm, "Cannot pop an empty list.")
		return nil_val(), false
	}

	return pop_value_array(&list.items), true
}

_partition :: proc(list: ^[dynamic]Value, lo, hi: int) -> int {
	random_pivot_idx := lo + cast(int)rand.int31() % (hi - lo + 1)

	tmp := list[random_pivot_idx]
	list[random_pivot_idx] = list[hi]
	list[hi] = tmp

	pivot := list[hi]
	idx := lo - 1
	pivot_as_num := as_number(pivot)

	for i := lo; i < hi; i += 1 {
		if as_number(list[i]) <= pivot_as_num {
			idx += 1
			tmp = list[i]
			list[i] = list[idx]
			list[idx] = tmp
		}
	}

	idx += 1
	list[hi] = list[idx]
	list[idx] = pivot

	return idx
}

_sort :: proc(list: ^[dynamic]Value, lo, hi: int) {
	if lo >= hi {
		return
	}

	pivot_idx := _partition(list, lo, hi)

	_sort(list, lo, pivot_idx - 1)
	_sort(list, pivot_idx + 1, hi)
}

/* Sort a list using the quicksort algorithm, and return the sorted list. */
sort_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_list(args[0]) {
		vm_panic(vm, "Cannot sort a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	list := as_list(args[0])
	_sort(&list.items.values, 0, list.items.count - 1)
	return args[0], true
}

/* Return the sum of the values in the list. */
sum_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_list(args[0]) {
		vm_panic(vm, "Cannot sum up a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	list := as_list(args[0])
	sum: f64 = 0
	for value in list.items.values {
		if !is_number(value) {
			vm_panic(vm, "Can only sum up a list of numbers.")
			return nil_val(), false
		}
		sum += as_number(value)
	}

	return number_val(sum), true
}

/* ------- RESULT ------- */

/* Actual name: "ok?"
Check if a provided result is the `ok` variant. */
is_ok_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_result(args[0]) {
		vm_panic(vm, "'ok?' only works on results, not %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	return bool_val(as_result(args[0]).is_ok), true
}

/* Actual name: "err?"
Check if a provided result is the `err` variant. */
is_err_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_result(args[0]) {
		vm_panic(vm, "'err?' only works on results, not %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	return bool_val(!as_result(args[0]).is_ok), true
}

/* Return the value wrapped in the first argument (a result) if it is the `ok` 
variant, and panic if it is the `err` variant. */
unwrap_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_result(args[0]) {
		vm_panic(vm, "Can only unwrap a Result, not a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	result := as_result(args[0])
	if !result.is_ok {
		vm_panic(vm, "Unwrapped an err variant.")
		return nil_val(), false
	}

	return result.value, true
}

/* Return a value wrapped in the first argument (a result) if it is the `ok` 
variant, and return the fallback second argument if it is the `err` variant. */
unwrap_or_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_result(args[0]) {
		vm_panic(vm, "Can only unwrap a Result, not a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	result := as_result(args[0])
	if !result.is_ok {
		return args[1], true
	}

	return result.value, true
}
