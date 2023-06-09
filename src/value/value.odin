package value

import "core:fmt"

/*
Type of a value in zen. Note that this is the VM's notion of a type, not of the
user, so each class the user defines, for instance, doesn't need its own entry
here.
*/
ValueType :: enum {}

/*
Representation of a zen value in Odin. Will soon use a tagged union to pack
the data type and the value's payload into a single place.
*/
Value :: f64

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

/* Print out `value` in a human-readable format. */
print_value :: proc (value: Value) {
    fmt.printf("%g", value)
}
