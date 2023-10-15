# zen

Reference for the zen programming language.

Get started now with the `print` statement to print to standard out:

```zen
print "Hello, world!"; //=> Hello, world!
```

Semicolons seperate statements. In the future, they'll likely become optional.

## Variables

Bind a value to a name with `var`.

```
var name = "Sam";
```

Use `val` to make the name single-assign.

```
val nice = 69;
nice = 68; // ERROR!
```

## Conditionals

### if-else

Use an `if` statement to execute some code if a condition is true.

```
if false {
    print "how did this happen!";
}
```

There is no need for parentheses between the condition, but the body MUST be
enclosed in braces.

Use an `else` statement following an `if` statement to execute code if the `if`
condition evaluates to false:

```
if false {
    print "how did this happen!";
} else {
    print "just another normal day";
}
```

### switch

zen has no `else if` / `elif` statement. However, the `switch` statement can be
used in such a situation. A `switch` statement checks for equality between a
selected value and an assortment of cases, from top to bottom. If a case matches,
the code associated with it is executed and the statement exits. A `switch true` 
can be used to easily emulate an `else if` statement.

```
var a = rand_int(0, 2);

switch a {
    0 => print "zero!";
    1 => print "one.";
    2 => {
        var y = 2;
        print y;
    }
    else => print "not between 0 and 2";
}
```

Note that the `else` clause is mandatory.

## Looping

zen has the traditional `while` and `for` loops.

```
var awesome = true;
while awesome {
    print "you're awesome!";
}

for var i = 0; i < 10; i = i + 1 {
    print "you're awesome " + i + "!";
}
```

## Functions

zen has powerful and flexible functions. All functions are first-class, so they can
be assigned to variables and passed to other functions.

Define a function with the `func` keyword, and call it with the `()` syntax:

```
func a_function() {
    print "this is a function!";
}
a_function(); //=> this is a function!
```

If a function is pure i.e. only returns a value, it can be shortened using JS-like
arrow notation:

```
func double(n) => n * 2;
```

Closures are also supported.

```
func outer() {
    let x = "outside";
    func inner() {
        print x;
        x = "inside";
        print x;
    }

    return inner;
}

final in = outer();
in(); // prints "outside" then "inside"
```

zen allows for anonymous function syntax as well, making it much easier to pass
functions around.

```
func apply(value, fn) {
    return fn(value);
}

print apply(2, func(n) { return n * 2; }); //=> 4
```

The above example can be made simpler using the convenient arrow notation:

```
print apply(2, func(n) => n * 2); //=> 4
```

## Pipes

zen supports a unique feature inspired by the Elixir programming language called
pipes. Pipes allow one to pass expressions to other expressions, or pass values
to functions.

```zen
// These two are equivalent:
print upcase("hello");
print "hello" |> upcase();
```

The last expression in a pipe sequence can be accessed using the `it` keyword.

```zen
print "68"
    |> parse()
    |> it + 1; // 69
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
- `parse(s)`: Attempt to parse a string `s` into a floating point number.

### errors

- `panic(s)`: Crash the program with a message `s`.

### io

- `gets()`: Read a line from stdin.

### strings

- `chomp(s)`: Trim whitespace from both sides of a string `s`.
- `len(s)`: Get the length of a string `s`.
- `replace(s, o, n)`: Replace all instances of a substring `o` in a string `s` with the substring `n`.
- `upcase(s)`: Turn the characters of a string `s` into uppercase.
- `downcase(s)`: Turn the characters of a string `s` into lowercase.
- `reverse(s)`: Reverse a string `s`.