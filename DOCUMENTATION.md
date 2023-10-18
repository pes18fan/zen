# zen

Reference for the zen programming language.

Get started now with the `print` statement to print to standard out:

```zen
print "Hello, world!\n" //=> Hello, world!
```

Note that the `print` statement does not append a newline. You can use the
puts() function if you want that.

Statements are seperated by semicolons. They are automatically inserted
only at newlines, following certain rules.

## Datatypes

zen has the following datatypes:

- `number`: A real number represented as a 64-bit floating point.
- `complex`: A complex number represented with two 64-bit floating points.
- `bool`: A boolean value i.e. true or false.
- `string`: A sequence of text.

You can create your own datatypes via classes.

## Variables

Bind a value to a name with `var`.

```
var name = "Sam"
```

Use `val` to make the name single-assign.

```
val nice = 69
nice = 68 // ERROR!
```

## Conditionals

### if-else

Use an `if` statement to execute some code if a condition is true.

```
if false {
    print "how did this happen!"
}
```

There is no need for parentheses between the condition, but the body MUST be
enclosed in braces.

Use an `else` statement following an `if` statement to execute code if the `if`
condition evaluates to false:

```
if false {
    print "how did this happen!"
} else {
    print "just another normal day"
}
```

### switch

zen has no `else if` / `elif` statement. However, the `switch` statement can be
used in such a situation. A `switch` statement checks for equality between a
selected value and an assortment of cases, from top to bottom. If a case matches,
the code associated with it is executed and the statement exits. A `switch true` 
can be used to easily emulate an `else if` statement.

```
var a = rand_int(0, 2)

switch a {
    0 => print "zero!"
    1 => print "one."
    2 => {
        var y = 2
        print y
    }
    else => print "not between 0 and 2"
}
```

Note that the `else` clause is mandatory.

## Looping

zen has the traditional `while` and `for` loops.

```
var awesome = true
while awesome {
    print "you're awesome!"
}

for var i = 0; i < 10; i = i + 1 {
    print "you're awesome " + i + "!"
}
```

## Functions

zen has powerful and flexible functions. All functions are first-class, so they can
be assigned to variables and passed to other functions.

Define a function with the `func` keyword, and call it with the `()` syntax:

```
func a_function() {
    print "this is a function!"
}
a_function() //=> this is a function!
```

If a function is pure i.e. only returns a value, it can be shortened using JS-like
arrow notation:

```
func double(n) => n * 2
```

Closures are also supported.

```
func outer() {
    var x = "outside"
    func inner() {
        print x
        x = "inside"
        print x
    }

    return inner
}

val in = outer()
in() // prints "outside" then "inside"
```

zen allows for anonymous function syntax as well, making it much easier to pass
functions around.

```
func apply(value, fn) {
    return fn(value)
}

print apply(2, func(n) { return n * 2; }) //=> 4
```

The above example can be made simpler using the convenient arrow notation:

```
print apply(2, func(n) => n * 2) //=> 4
```

## Pipes

zen supports a unique feature inspired by the Elixir programming language called
pipes. Pipes allow one to pass expressions to other expressions, or pass values
to functions.

```zen
// These two are equivalent:
print upcase("hello")
print "hello" |> upcase()
```

The last expression in a pipe sequence can be accessed using the `it` keyword.

```zen
print "68"
    |> parse()
    |> it + 1 // 69
```

## Lists

zen has lists; called dynamic arrays in other languages, to store values.

Lists can be created using a list literal:

```zen
var list = [1, 2, 3]
```

The list can be subscripted with a positive integer index to get an element out
of it.

```zen
list[0] //=> 1
```

The `push()` and `pop()` native functions can be used to add and remove items
from a list, and the `len()` function can be used to get the length of the list.

## Classes

Classes can be used to create your own datatypes.

Declare a class as follows:

```zen
class Pair {}
```

You can create instances out of such a class by calling it:

```zen
var pair = Pair()
```

Fields can be freely added to an instance.

```zen
pair.first = 1
pair.second = 2
puts(pair.first + pair.second) //=> 3
```

## Standard library functions

zen has several functions that are present for you to use in the standard library.
These include the following:

### time

- `clock()`: Get the current UNIX time in seconds.

### math

- `sqrt(x)`: Find the square root of a positive real number `x`.
- `ln(x)`: Find the natural log of a positive real number `x`.
- `pow(x, n)`: Find the result of raising `x` to the `n`th power.
- `floor(x)`: Find the largest integer less than `x`.
- `ceil(x)`: Find the smallest integer greater than `x`.
- `round(x)`: Round `x` to the nearest integer.

#### complex

- `complex(r, i)`: Create a complex number with real part `r` and imaginary part `i`.
- `conjg(z)`: Find the complex conjugate of a number `z`.
- `real(z)`: Get the real part of a complex number `z`.
- `imag(z)`: Get the imaginary part of a complex number `z`.
- `abs(z)`: Find the absolute value of a real number or the magnitude of a complex number.

### errors

- `panic(s)`: Crash the program with a message `s`.

### io

- `puts(x)`: Print an expression to stdin and append a newline.
- `gets()`: Read a line from stdin.

### strings

- `chomp(s)`: Trim whitespace from both sides of a string `s`.
- `len(s)`: Get the length of a string or list `s`.
- `replace(s, o, n)`: Replace all instances of a substring `o` in a string `s` with the substring `n`.
- `upcase(s)`: Turn the characters of a string `s` into uppercase.
- `downcase(s)`: Turn the characters of a string `s` into lowercase.
- `reverse(s)`: Reverse a string `s`.

### lists

- `len(l)`: Get the length of a list or string `l`.
- `push(l, i)`: Add an item `i` to the end of the list `l`.
- `pop(l)`: Get the last item of the list `l` after removing it from the list.

### types and conversion

- `typeof(x)`: Get the type of any expression.
- `str(x)`: Convert any value into a string.
- `parse(s)`: Attempt to parse a string `s` into a floating point number.
