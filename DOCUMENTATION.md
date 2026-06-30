# zen

Reference for the zen programming language.

Get started now with the `print` expression to print to standard out:

```zen
print "Hello, world!\n" //=> Hello, world!
```

Note that the `print` expression does not append a newline. You can use the
`puts()` function if you want that.

Every piece of code in zen is an **expression** that produces a value. Statements
like `if`, `while`, `for`, `switch`, blocks `{ ... }`, and variable declarations
`var`/`val` all produce values and can be composed together. In fact, a zen
program is in itself one large expression.

## Semicolons

Expressions are chained together by semicolons. The semicolon also discards the
value of the expression before it, causing it to evaluate to `nil` within the
surrounding sequence. For instance, `1` evaluates to the number 1 but `1;` 
evaluates to `nil` by discarding the number value.

A collection of expressions joined by semicolons is called a **sequence**. A
sequence can either end with an expression or a semicolon. A sequence itself 
is an expression; it evaluates to whatever expression is at its end, or to `nil`
if it ends with a semicolon.

Sequences can only exist at the top level of the file or at the top level of
a block.

## Datatypes

zen has the following primitive datatypes:

- `Number`: A real number represented as a 64-bit floating point. Numbers also
    support exponential notation (e.g `1e2` for `100`).
- `Bool`: A boolean value i.e. true or false.
- `String`: A sequence of text, enclosed by either double or single quotes.
    Strings are immutable in zen.
- `Nil`: A value that represents the absence of a value. It is the default
    value for uninitialized variables and the implicit return value for functions
    that do not return anything.

## Type system

zen is statically typed. It uses Hindley-Milner type inference, allowing it
to automatically infer types of expressions. However, type annotations may
still be provided where desired for readability or to constrain inference.

## Variables

Create a new variable using `var`.

```
var name = "Sam"
```

Use `val` to make the name single-assign.

```
val nice = 69;
nice = 68 // ERROR!
```

With `val` however, only the binding is final; objects like lists and instances
can still be mutated.

Variables are lexically scoped.

Variable names can include any letter in the English alphabet, underscores,
numbers and question marks. However, a variable must begin with a letter or
underscore.

```zen
var _ = "something";
val truthy? = true;
var name123 = "some name"
```

Multiple variables can be declared together using comma separation on the same
line or spanning multiple lines. A `var` or `val` declaration returns `nil`.

```
var
    age = 21,
    is_cool = true,
    unknown
```

Uninitialized variables default to `nil`.

> [!NOTE]
> Like in Python, zen uses aliasing. What that means is that doing
> something like `b = a` causes `b` and `a` to both refer to the same object in
> memory, rather than creating a new copy. So, any changes to `a` will also be
> reflected in `b`. However, this is only true for non-immutable types.
>
> Aliasing can cause unexpected results, specifically in the case
> of lists which are mutable. For more information, see the section on lists.

## Blocks

A block `{ ... }` is an expression that groups multiple expressions together and
produces the value of its last expression. Blocks create a new lexical scope, such
that variables declared inside a block are not visible outside it.

```
var x = {
    var y = 2;
    y * 10
};
puts(x) //=> 20
```

## Conditionals

### if-else

Use an `if` expression to evaluate some code if a condition is true.

```
if false {
    print "how did this happen!"
}
```

There is no need for parentheses around the condition, but the body MUST be
enclosed in braces.

Optionally, use an `else` branch following an `if` to execute code if the condition
evaluates to false:

```
if false {
    print "how did this happen!"
} else {
    print "just another normal day"
}
```

The entire `if`/`else` expression produces the value of whichever branch is taken.
An `if` without an `else` branch produces `nil`.

### switch

zen has no `else if` / `elif` expression. However, `switch` can be used in such
a situation. A `switch` expression checks for equality between a selected value
and an assortment of cases, from top to bottom. If a case matches, the expression
associated with it is evaluated and returned as the value of the entire expression.

```
use "math";

var a = math.rand() * 10 |> math.floor();

switch a {
    0 => print "zero!",
    1 => print "one.",
    2 => {
        var y = 2;
        print y
    },
    else => print "not between 0 and 2"
}
```

An `else` clause at the end is mandatory, and is evaluated if none of the other
clauses match.

Omitting the value after `switch` is shorthand for `switch true`. This can be
used to easily emulate an `else if` expression:

```zen
switch {
    0 == 0 => puts "zero is zero!",
    0 == 1 => puts "zero is on- wait what?",
    else => puts "ok I don't even know anymore"
}
```

## Loops

zen has the traditional `while` and `for` loops.

```
var awesome = true;
while awesome {
    print "you're awesome!"
};

for var i = 0; i < 10; i = i + 1 {
    print "you're awesome " .. i .. "!"
}
```

Additionally, zen also provides the more modern `for in` loops. They can be
used to loop over a list element-by-element.

```
for i in [1, 2, 3] {
    puts(i)
}
```

You can also iterate over strings by converting them into lists:

```
use "string";

for x in string.chars("hello") {
    puts(x)
}
```

There are two expressions used for loop control:

- `break` exits the innermost loop
- `continue` skips to the next iteration.

Both can only be used inside a loop.

All loops in zen evaluate to `nil`.

## Functions

zen has powerful and flexible functions. All functions are first-class, so they can
be assigned to variables, passed to other functions and returned from functions.

Define a function with the `func` keyword, and call it with the `()` syntax.
A named function declaration like `func name() { ... }` is syntactic sugar over
`var name = func() { ... }`.

```
func a_function() {
    print "this is a function!"
};
a_function() //=> this is a function!
```

Functions return either the final expression in their block or a value from
an explicit `return`, whichever comes first.

If a function only returns a value, it can be shortened using JS-like
arrow notation:

```
func double(n) => n * 2;
```

Closures are also supported.

```
func outer() {
    var x = "outside";
    func inner() {
        print x;
        x = "inside";
        print x
    };

    return inner
};

val in = outer();
in() // prints "outside" then "inside"
```

zen allows for anonymous function syntax as well, making it much easier to pass
functions around.

```
func apply(value, fn) {
    return fn(value)
};

print apply(2, func(n) { return n * 2 }) //=> 4
```

The above example can be made simpler using the convenient arrow notation:

```
print apply(2, func(n) => n * 2) //=> 4
```

Additionally, if a function has a single argument which is a string, you can omit
the parentheses.

```zen
puts "hey, no parens!"
```

Named functions (including both declaration syntactic sugar and named lambdas) 
are hoisted when defined in the global scope. This means that a function in the
global scope can be used before it is defined, which is incredibly useful for
free ordering of declarations as well as for mutual recursion purposes.

```zen
func is_even(n) => if n == 0 { true } else { is_odd(n - 1) }
func is_odd(n) => if n == 0 { false } else { is_even(n - 1) }

puts(is_even(4)) //=> true
puts(is_odd(4))  //=> false
```

### Generic functions

zen also supports generic functions based on its Hindley-Milner-style type 
inference.

You can introduce type parameters after the function name:

```zen
func name[T, U](arg1: T, arg2: U): T {
    arg1
}
```

All type annotations are optional. The type parameters themselves are optional
too, because zen can often infer them automatically.

```zen
func id(x) {
    x
}

func id[T](x: T): T {
    x
}
```

A generic function can use its type parameters in argument types, return types,
or both. Type parameters can also be unused if the function does not need them
directly.

```zen
func first[A, B](a: A, b: B): A {
    a
}

func ignore[T](x: T) {
    1
}
```

Type parameters may be inferred from call sites, so the same function can be used
at multiple concrete types without needing separate overloads.

```zen
func wrap(x) {
    x
};

puts(wrap(1))     //=> 1
puts(wrap("hi"))   //=> hi
puts(wrap(0.25))  //=> 0.250
```

Generic functions may also be nested, and inner type parameters follow normal
lexical scoping rules.

```zen
func outer[T](x: T) {
    func inner[T](y: T): T {
        y
    };

    inner(x)
};
```

If a generic constraint cannot be satisfied, the compiler reports a type error
as usual.

## Operators

zen has all of the common operators, including `+`, `-`, `*`, `/` and `%` for 
numeric operations,  `==`, `!=`, `>`, `<`, `>=`, `<=` for comparisons, `and`, 
`or` and `not` for boolean operations, as well as the `..` for string 
concatenation, and the pipe operator `|>` described in more detail in the next
section.

## Pipelines

zen supports a unique feature inspired by the Elixir programming language called
the pipe operator. This operator allow one to pass expressions to other expressions,
or pass values to functions; in chains known as pipelines.

```zen
use "string";

// These two are equivalent:
print string.upcase("hello");
print "hello" |> string.upcase()
```

When the right side of a pipe is a function call, the piped value is implicitly
passed as the first argument. The previous expression in a pipeline can also be
accessed using the `it` keyword, which is especially useful when passing the
value to a non-function expression.

```zen
print "68"
    |> parse()
    |> it + 1 // 69
```

The `it` keyword is only valid inside a pipe expression; using it outside a
pipe produces a compile error.

## Lists

A list is a ordered, indexable sequence of values.

Lists have the type `List[a]`, where the `a` represents the type of the
values in the list. This means that lists in zen are homogenous; only one type
of value is allowed in a list.

Lists can be created using a list literal:

```zen
var list = [1, 2, 3]
```

The list can be subscripted with a non-negative integer index to get an element out
of it.

```zen
list[0] //=> 1
```

The `push()` and `pop()` native functions in the `list` module can be used to 
add and remove items from a list, and the global `len()` native function can be 
used to get the length of the list.

You can set a value at a specific index of the list by using the subscript syntax
alongside an assignment.

```zen
var list = [1, 2, 3];
list[0] = 4;
puts(list)  //=> [4, 2, 3]
```

Lists work well with pipelines as well!

```zen
[4, 3, 2, 1]
    |> list.push(5)
    |> list.push(6)
    |> list.remove_last()
    |> list.sort()  //=> [1, 2, 3, 4, 5]
```

> [!NOTE]
> All the functions provided by the list module mutate the provided list. 
> Therefore, if a list bound to a variable is passed through a pipeline, it will
> be mutated, alongside any other variable it references. This is done because
> keeping the passed variable the same would necessitate copying lists on every
> pipeline stage, which would be very expensive.
>
> ```zen
> var a = [1, 2, 3];
> var b = a |> list.push(4);
> puts(a)   //=> [1, 2, 3, 4]
> ```
> 
> To avoid this, you can explicitly copy the list before passing it through the
> pipeline.
>
> ```zen
> var a = [1, 2, 3];
> var b = copy(a) |> list.push(4);
> puts(a)   //=> [1, 2, 3]
> ```

## Modules

Modules can be imported using the `use` keyword followed by a string path. Modules
are of two types, builtin and user-defined modules.

### Builtin modules

Builtin modules are a set of modules built into the language itself with various
useful functions. A builtin module can be imported by simply using `use "mod"`
where `mod` is the name of the module. Any function in the builtin module can be
accessed and called using dot notation like with instances.

```zen
use "time";

puts(time.clock())
```

Further information on what builtin modules are present is provided below.

### User-defined modules

User-defined modules basically mean a file of code that can be imported with
`use`. It will run the file and package all of its functions prefixed
with the `pub` keyword within the imported file's name (e.g. the module will
be called `foo` if the imported file is `foo.zn`.)

```zen
// a.zn
use "./b.zn";

b.foo()
```

```zen
// b.zn
pub func foo() {
    puts("bar")
}
```

Running `a.zn` will print out "bar".

Functions without the `pub` keyword will NOT be imported when a file is `use`d.

## Error handling

zen uses the builtin `Result` type for error handling. It is a type with one
of two variants: `ok` or `err`.

Either of these variants can be created using the builtin functions `ok`
and `err`.

```zen
var k = ok(1);
var e = err("some error")
```

The `result` builtin module has a couple of functions for working with results.

You can check if a result is an `ok` variant or an `err` variant using the
`result.ok?` and `result.err?` predicates.

```zen
use "result";

result.ok?(ok(1));                  //=> true
result.ok?(err("something bad"));   //=> false
result.err?(ok("all good"));        //=> false
result.err?(err(-1))                //=> true
```

The `result.unwrap` native function takes in a result and returns the value inside it
if it is an `Ok` variant. If it is a `Err` variant, the function will panic.

```zen
use "result"

result.unwrap(ok(1));         //=> 1
result.unwrap(err("uh oh"))   //=> panic: Unwrapped an Err variant.
```

To avoid panicking, you can use the `unwrap_or` function to provide a fallback.

```zen
use "result"

result.unwrap_or(ok(1), 2);          //=> 1
result.unwrap_or(err("uh oh"), -1)   //=> -1
```

## Exiting early

You can use the `exit` expression to exit a program early.

```
puts "hello";
exit;
puts "world"
```

This will print "hello" and exit.

You can add a number after `exit` to exit with that status code. Without any
number, it defaults to a status code of 0 (success).

## Standard library

zen's standard library contains various modules and some globally available functions
for you to use.

### Functions in the global scope

- `puts(x)`: Print an expression to stdout and append a newline.
- `gets()`: Read a line from stdin.
- `len(s)`: Get the length of a string or list `s`.
- `typeof(x)`: Get the type of any expression as a string.
- `str(x)`: Convert any value into a string.
- `parse(s)`: Attempt to parse a string `s` into a floating point number. Panics
    if the string is not a valid number.
- `copy(x)`: Return a copy of `x`.
- `panic(s)`: Crash the program with a message `s`.
- `assert(x)`: Crash the program if the expression `x` is falsey, otherwise do
    nothing.
- `dirname()`: Return the directory containing the running program, or an empty 
    string if running a REPL.
- `filename()`: Return the name of the running program, or an empty string if 
    running a REPL.
- `ok(x)`: Return the `ok` variant of `Result` wrapping the value `x`.
- `err(x)`: Return the `err` variant of `Result` wrapping the value `x`.

### module `time`

- `clock()`: Get the current UNIX time in seconds.
- `clock_ms()`: Get the current UNIX time in milliseconds.

### module `math`

- `sin(x)`: Find the sine of a real number `x`.
- `cos(x)`: Find the cosine of a real number `x`.
- `tan(x)`: Find the tangent of a real number `x`.
- `sqrt(x)`: Find the square root of a positive real number `x`.
- `ln(x)`: Find the natural log of a positive real number `x`.
- `pow(x, n)`: Find the result of raising `x` to the `n`th power.
- `floor(x)`: Find the largest integer less than `x`.
- `ceil(x)`: Find the smallest integer greater than `x`.
- `round(x)`: Round `x` to the nearest integer.
- `abs(x)`: Find the absolute value of a real number.
- `rand()`: Create a random double in the interval [0, 1).

### module `os`

- `read(p)`: Read a file at the path `p` and return the contents as a string.
    Panics if the file doesn't exist.
- `write(p, m, s)`: Write the string `s` to a file `p` in the mode `m`. This 
    function returns `nil`. If the file does not exist, it will be created. The
    mode is a string and may be one of the following:
    - `"w"`: To overwrite the file's contents.
    - `"a"`: To append to the file.

> [!NOTE]
> The path for the read and write functions is relative to the running program. 
> If in a REPL, the path is relative to wherever the REPL was invoked.

- `args()`: Get the arguments passed to the program as a list.

### module `string`

- `chomp(s)`: Trim whitespace from both sides of a string `s`.
- `replace(s, o, n)`: Replace all instances of a substring `o` in a string `s`
    with the substring `n`.
- `slice(s, start, end)`: Get a subscring of the string `s` between the indices
    `start` and `end` (exclusive). Panics if the indices are out of bounds or
    if they're not valid integers.
- `index(s, k)`: Get the `k`th character (UTF-8 codepoint) of the string `s`.
- `chars(s)`: Turn the string `s` into a list consisting of strings, each
    being one character (UTF-8 codepoint) of the string.
- `upcase(s)`: Turn the characters of a string `s` into uppercase.
- `downcase(s)`: Turn the characters of a string `s` into lowercase.
- `reverse(s)`: Reverse a string `s`.
- `asciichar(x)`: Convert a number to its corresponding ASCII character.
- `asciinum(s)`: Convert an ASCII character to its corresponding number.

### module `list`

- `push(l, i)`: Add an item `i` to the end of the list `l` and return the new list.
- `pop(l)`: Get the last item of the list `l` after removing it from the list.
- `remove_last(l)`: Remove the last item of the list `l` and return the list.
- `sort(l)`: Sort the list `l` and return it.

> [!NOTE]
> All these functions mutate the list in-place and return the mutated list.

- `sum(l)`: Reduce a list of numbers to its sum.

### module `result`

- `unwrap(r)`: Get the wrapped value of the result `r` if it is the `ok` variant,
    and panic if it is the `err` variant.
- `unwrap_or(r, x)`: Get the wrapped value of the result `r` if it is the `ok` variant,
    or the fallback value `x` if it is the `err` variant.

## Features that used to exist

### Classes

zen had a basic OOP system inherited from clox, with classes and inheritance.
This however was removed as the language became more functional-styled.

## The chaotic stuff

You can get these additional cursed features by compiling `zen` while defining
`CHAOTIC` as `true` in the Odin compiler. The easiest way to make a chaotic build
is to simply run `./x.py chaotic` in the root directory and you'll get your build
at `./bin/chaotic/zen`.

These features will **NOT** be tested or given much thought to, since they're just
funny little things rather than anything serious.

### ifn't and whilen't

The opposite of `if` and `while`.

Just as brain-spinning as `unless` and `until` in Ruby.

```zen
ifn't true {
    puts("this will never run")
}

whilen't i == 11 {
    puts(i)
    i = i + 1
}
```
