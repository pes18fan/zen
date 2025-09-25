package zen

import "core:fmt"
import "core:math"
import "core:math/rand"
import "core:os"
import "core:path/filepath"
import "core:strconv"
import "core:strings"
import "core:time"

BuiltinModule :: enum {
	TIME,
	MATH,
	OS,
	STRING,
	LIST,
}

ModuleFunction :: struct {
	name:     string,
	function: NativeFn,
	arity:    int,
}

/* Initialize the list of builtin modules. */
init_builtin_modules :: proc(gc: ^GC) {
	append(&gc.std_modules, "time", "math", "os", "string", "list")
}

/* Get all the information required to import a builtin module into the global
 * scope. */
get_builtin_module :: proc(gc: ^GC, module_name: BuiltinModule) -> []ModuleFunction {
	module_functions := make([dynamic]ModuleFunction)

	#partial switch module_name {
	case .TIME:
		{
			append(&module_functions, ModuleFunction{"clock", clock_native, 0})
		}
	case .MATH:
		{
			append(&module_functions, ModuleFunction{"sqrt", sqrt_native, 1})
			append(&module_functions, ModuleFunction{"ln", ln_native, 1})
			append(&module_functions, ModuleFunction{"pow", pow_native, 2})
			append(&module_functions, ModuleFunction{"floor", floor_native, 1})
			append(&module_functions, ModuleFunction{"ceil", ceil_native, 1})
			append(&module_functions, ModuleFunction{"round", round_native, 1})
			append(&module_functions, ModuleFunction{"abs", abs_native, 1})
			append(&module_functions, ModuleFunction{"rand", rand_native, 0})
		}
	case .OS:
		{
			append(&module_functions, ModuleFunction{"panic", panic_native, 1})
			append(&module_functions, ModuleFunction{"read", read_native, 1})
			append(&module_functions, ModuleFunction{"write", write_native, 3})
			append(&module_functions, ModuleFunction{"args", args_native, 0})
		}
	case .LIST:
		{
			append(&module_functions, ModuleFunction{"push", push_native, 2})
			append(&module_functions, ModuleFunction{"pop", pop_native, 1})
			append(&module_functions, ModuleFunction{"sort", sort_native, 1})
		}
	case .STRING:
		{
			append(&module_functions, ModuleFunction{"chomp", chomp_native, 1})
			append(&module_functions, ModuleFunction{"replace", replace_native, 3})
			append(&module_functions, ModuleFunction{"upcase", upcase_native, 1})
			append(&module_functions, ModuleFunction{"downcase", downcase_native, 1})
			append(&module_functions, ModuleFunction{"reverse", reverse_native, 1})
			append(&module_functions, ModuleFunction{"asciichar", asciichar_native, 1})
			append(&module_functions, ModuleFunction{"asciinum", asciinum_native, 1})
		}
	}

	return module_functions[:]
}

/* These are the functions available in the global scope. Only five very commonly used
 * functions are available as such. The rest are in their corresponding modules. */
init_natives :: proc(gc: ^GC) {
	/* Add all the names of the globally present native functions to the name list. */
	append(
		&gc.global_native_fns,
		"puts",
		"gets",
		"len",
		"str",
		"parse",
		"typeof",
		"copy",
		"dirname",
		"filename",
	)

	// io
	define_native(gc, "puts", puts_native, arity = 1)
	define_native(gc, "gets", gets_native, arity = 0)

	define_native(gc, "len", len_native, arity = 1)

	// types and conversion
	define_native(gc, "str", str_native, arity = 1)
	define_native(gc, "parse", parse_native, arity = 1)
	define_native(gc, "typeof", typeof_native, arity = 1)

	// mem
	define_native(gc, "copy", copy_native, arity = 1)

	// others
	define_native(gc, "dirname", dirname_native, arity = 0)
	define_native(gc, "filename", filename_native, arity = 0)
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
		vm_panic(vm, "Failed to read input.")
		return nil_val(), false
	}

	return obj_val(copy_string(vm.gc, string(buf[:n]))), true
}


/* Convert any value to a string. */
str_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	str, was_allocation := stringify_value(args[0])
	defer if was_allocation {
		delete(str)
	}
	return obj_val(copy_string(vm.gc, str)), true
}

/* Return the type of any value, represented as a string. */
typeof_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return obj_val(copy_string(vm.gc, type_of_value(args[0]))), true
}

_copy_item :: proc(vm: ^VM, value: Value) -> Value {
	if !is_obj(value) {
		return value
	}

	obj := as_obj(value)

	#partial switch obj.type {
	case .INSTANCE:
		{
			instance := as_instance(obj_val(obj))
			new := new_instance(vm.gc, instance.klass)

			/* Deep copy the fields */
			table_add_all(from = &instance.fields, to = &new.fields)
			return obj_val(new)
		}
	case .LIST:
		{
			list := as_list(obj_val(obj))
			new := new_list(vm.gc)

			/* Copy all the list's items over. */
			for i := 0; i < list.items.count; i += 1 {
				/* _copy_item may be called recursively if we're deep copying a
                 * list within a list. */
				write_value_array(&new.items, _copy_item(vm, list.items.values[i]))
			}

			return obj_val(new)
		}
	case:
		return value
	}

	return nil_val()
}

/* Copy an object. */
copy_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return _copy_item(vm, args[0]), true
}

/* Return the directory containing the running program. Returns an empty string
 * if in a REPL. */
dirname_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return obj_val(copy_string(vm.gc, config.__dirname)), true
}

/* Return the name of the running program. Returns an empty string if in a REPL. */
filename_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return obj_val(copy_string(vm.gc, config.__path)), true
}

/* ---------- TIME ---------- */

/* Get the current UNIX time in seconds. */
clock_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	return number_val(f64(time.to_unix_seconds(time.now()))), true
}

/* ---------- OS ---------- */

/* Panic the VM with a custom message. */
panic_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Panic message must be a string, not a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	vm_panic(vm, "%s", as_cstring(args[0]))
	return nil_val(), false
}

/* Read a file and return its data. */
read_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "The given path %v must be a string.", type_of_value(args[0]))
		return nil_val(), false
	}

	path := as_string(args[0]).chars
	abs_path := filepath.join([]string{config.__dirname, path})
	defer delete(abs_path)

	data, ok := os.read_entire_file_from_filename(abs_path)
	defer delete(data)
	if !ok {
		vm_panic(vm, "Failed to read file '%s'. Does it exist?", abs_path)
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
	abs_path := filepath.join([]string{config.__dirname, path})
	defer delete(abs_path)

	mode := as_string(args[1]).chars
	data := as_string(args[2]).chars

	if mode == "w" {
		ok := os.write_entire_file(abs_path, transmute([]u8)data)
		if !ok {
			vm_panic(vm, "Failed to write to file '%s'.", path)
			return nil_val(), false
		}

		return nil_val(), true
	} else if mode == "a" {
		/* Without the S_IRUSR and S_IWUSR, the user won't be able to read or
         * write to the file at all. */

		flags: int
		flags = os.O_APPEND | os.O_RDWR | os.O_CREATE

		when ODIN_OS != .Windows {
			flags |= os.S_IRUSR | os.S_IWUSR
		}

		f, oerr := os.open(abs_path, flags)
		if oerr != nil {
			vm_panic(vm, "Failed to open file '%s' for appending.", path)
			return nil_val(), false
		}
		defer os.close(f)

		_, werr := os.write(f, transmute([]u8)data)
		if werr != nil {
			vm_panic(vm, "Failed to write to file '%s'.", path)
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

	n, ok := strconv.parse_f64(as_cstring(args[0]))
	if !ok {
		vm_panic(vm, "Cannot parse '%s' to a real number.", as_string(args[0]).chars)
		return nil_val(), false
	}

	return number_val(n), true
}

/* ---------- MATH ---------- */

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
			"All arguments for gsub must be strings, got %v instead.",
			type_of_value(args[0]),
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

/* Trim whitespace from both sides of a string. */
chomp_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "Cannot chomp a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	return obj_val(copy_string(vm.gc, strings.trim_space(as_string(args[0]).chars))), true
}

/* Turn all the characters of a string into uppercase. */
upcase_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_string(args[0]) {
		vm_panic(vm, "upcase() requires a string, not a %v.", type_of_value(args[0]))
		return nil_val(), false
	}

	str, err := strings.to_upper(as_string(args[0]).chars)
	if err != nil {
		vm_panic(vm, "Allocator error on upcase(): %s", err)
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
		vm_panic(vm, "Allocator error on downcase(): %s", err)
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

	str, err := strings.reverse(as_cstring(args[0]))
	if err != nil {
		vm_panic(vm, "Allocator error on reverse(): %s", err)
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
	return nil_val(), true
}

/* Pop a value off a list and return it. */
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
	// TODO: Find out why using a random pivot doesn't work
	// random_pivot_idx := cast(int)rand.int31() % (hi + 1)
	//
	// tmp := list[random_pivot_idx]
	// list[random_pivot_idx] = list[hi]
	// list[hi] = tmp

	pivot := list[hi]
	idx := lo - 1
	pivot_as_num := as_number(pivot)

	for i := lo; i < hi; i += 1 {
		if as_number(list[i]) <= pivot_as_num {
			idx += 1
			tmp := list[i]
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

/* Sort a list using the quicksort algorithm. */
sort_native :: proc(vm: ^VM, arg_count: int, args: []Value) -> (Value, bool) {
	if !is_list(args[0]) {
		vm_panic(vm, "Cannot sort a %v.", type_of_value(args[0]))
		return nil_val(), false
	}


	list := as_list(args[0])

	_sort(&list.items.values, 0, list.items.count - 1)

	return nil_val(), true
}
