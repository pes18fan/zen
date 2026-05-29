# zen

Reference for the zen programming language.

Get started now with the `print` expression to print to standard out:

```zen
print "Hello, world!\n" //=> Hello, world!
```

Note that the `print` expression does not append a newline. You can use the
`puts()` function if you want that.

Expressions are seperated by newlines. Semicolons can be used to separate
expressions on a single line. Newlines are suppressed when inside parentheses,
within lists, and when the next token continues the expression (e.g. after `+`,
`,`, `|>`, etc.).

Every piece of code in zen is an **expression** that produces a value. Statements
like `if`, `while`, `for`, `switch`, blocks `{ ... }`, and variable declarations
`var`/`val` all produce values and can be composed together. In fact, a zen
program is in itself one large expression.

## Datatypes

zen has the following primitive datatypes:

- `number`: A real number represented as a 64-bit floating point. Numbers also
    support exponential notation (e.g `1e2` for `100`).
- `bool`: A boolean value i.e. true or false.
- `string`: A sequence of text, enclosed by either double or single quotes.
    Strings are immutable in zen.
- `nil`: A value that represents the absence of a value. It is the default
    value for uninitialized variables and the implicit return value for functions
    that do not return anything.

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

With `val` however, only the binding is final; objects like lists and instances
can still be mutated.

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
> of lists and class instances, which are both mutable. For more information, 
> see the section on lists.

## Exiting early

You can use the `exit` expression to exit a program early.

```
puts "hello"
exit
puts "world"
```

This will print "hello" and exit.

You can add a number after `exit` to exit with that status code. Without any
number, it defaults to a status code of 0 (success).

## Blocks

A block `{ ... }` is an expression that groups multiple expressions together and
produces the value of its last expression. Blocks create a new lexical scope, such
that variables declared inside a block are not visible outside it.

```
var x = {
    var y = 2
    y * 10
}
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
use "math"

var a = math.rand() * 10 |> math.floor()

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

An `else` clause at the end is mandatory, and is evaluated if none of the other
clauses match.

A `switch true` can be used to easily emulate an `else if` expression:

```zen
switch {
    0 == 0 => puts "zero is zero!"
    0 == 1 => puts "zero is on- wait what?"
    else => puts "ok I don't even know anymore"
}
```

## Looping

zen has the traditional `while` and `for` loops. All loops produce `nil`.

```
var awesome = true
while awesome {
    print "you're awesome!"
}

for var i = 0; i < 10; i = i + 1 {
    print "you're awesome " + i + "!"
}
```

Additionally, zen also provides the more modern `for in` loops. They can be
used to loop over either a list or a string element-by-element.

```
for i in [1, 2, 3] {
    puts(i)
}

for x in "hello" {
    puts(x)
}
```

There are two expressions used for loop control:

- `break` exits the innermost loop
- `continue` skips to the next iteration.

Both can only be used inside a loop.

## Functions

zen has powerful and flexible functions. All functions are first-class, so they can
be assigned to variables and passed to other functions.

Define a function with the `func` keyword, and call it with the `()` syntax.
A named function declaration like `func name() { ... }` is syntactic sugar over
`var name = func() { ... }`.

```
func a_function() {
    print "this is a function!"
}
a_function() //=> this is a function!
```

Functions return either the final expression in their block or a value from
an explicit `return`, whichever comes first.

If a function only returns a value, it can be shortened using JS-like
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

## Pipelines

zen supports a unique feature inspired by the Elixir programming language called
the pipe operator. This operator allow one to pass expressions to other expressions,
or pass values to functions; in chains known as pipelines.

```zen
use "string"

// These two are equivalent:
print string.upcase("hello")
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
var list = [1, 2, 3]
list[0] = 4
puts(list)  //=> [4, 2, 3]
```

> [!NOTE]
> You can also use the subscripting syntax on strings to get a character at the
> provided index. However, subscript assignment is not allowed for strings
> as they are immutable.

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
> var a = [1, 2, 3]
> var b = a |> list.push(4)
> puts(a)   //=> [1, 2, 3, 4]
> ```
> 
> To avoid this, you can explicitly copy the list before passing it through the
> pipeline.
>
> ```zen
> var a = [1, 2, 3]
> var b = copy(a) |> list.push(4)
> puts(a)   //=> [1, 2, 3]
> ```

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

Methods can be added inside the body of a class. A constructor can also be
created for the class, which must have the name `init`. When you call a class,
the arguments you pass to it are sent to the constructor. To access the instance
within methods, use `this`.

```zen
class CoffeeMaker {
    init(coffee) {
        this.coffee = coffee
    }

    brew() {
        print "Enjoy your cup of " + this.coffee

        // No reusing the grounds!
        this.coffee = nil
    }
}

var maker = CoffeeMaker("coffee and chicory")
maker.brew()
```

Classes also support inheritance, where you can use `<` while defining a class
to make it inherit from another. A subclass will inherit its parent class's
methods. The `super` keyword followed by a dot and method name can be used
to access the method as defined in the inheriting class's parent class.

```zen
class Animal {
    init(name) {
        this.name = name
    }
}

class Dog < Animal {
    init(name) {
        super.init(name)
    }

    bark() {
        puts(this.name + " says woof!")
    }
}

val rover = Dog("Rover")
rover.bark() // Rover says woof!
```

## Modules

Modules can be imported using the `use` keyword followed by a string path. Modules
are of two types, builtin and user-defined modules.

### Builtin modules

Builtin modules are a set of modules built into the language itself with various
useful functions. A builtin module can be imported by simply using `use "mod"`
where `mod` is the name of the module. Any function in the builtin module can be
accessed and called using dot notation like with instances.

```zen
use "time"

puts(time.clock())
```

Further information on what builtin modules are present is provided below.

### User-defined modules

User-defined modules basically mean a file of code that can be imported with
`use`. It will run the file and package all of its functions and classes prefixed
with the `pub` keyword within the imported file's name (e.g. the module will
be called `foo` if the imported file is `foo.zn`.)

```zen
// a.zn
use "./b.zn"

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

Classes can similarly be shared between files:

```zen
// a.zn
use "b.zn"

b.Foo().bar()
```

```zen
// b.zn
pub class Foo {
    bar() {
        puts("baz")
    }
}
```

## Discard

The `discard` keyword turns the expression following it into `nil`.

```zen
discard 1               //=> nil
discard "hi"            //=> nil
discard SomeClass()     //=> nil
discard func(x) => x    //=> nil
```

This is especially useful when you don't want to return an explicit value from
a block or a function.

```zen
var x = 0

func normal() {
    x = 1
}

func discarder() {
    discard x = 1
}

puts(normal())      //=> 1
puts(discarder())   //=> nil
```

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
- `dirname()`: Return the directory containing the running program, or an empty 
    string if running a REPL.
- `filename()`: Return the name of the running program, or an empty string if 
    running a REPL.

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

- `panic(s)`: Crash the program with a message `s`.
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
