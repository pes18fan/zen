# zen

Reference for the zen programming language.

Get started now with the `print` statement to print to standard out:

```zen
print "Hello, world!\n" //=> Hello, world!
```

Note that the `print` statement does not append a newline. You can use the
`puts()` function if you want that.

Statements are seperated by semicolons. They are automatically inserted
only at newlines, following [certain rules](https://github.com/pes18fan/zen/blob/0b72dd5fa5d59a5fc42685c19ca82d48fb72cb93/src/lexer.odin#L170C1-L170C1).

You can prevent automatic semicolon insertion by using a backslash.

## Datatypes

zen has the following datatypes:

- `number`: A real number represented as a 64-bit floating point.
- `bool`: A boolean value i.e. true or false.
- `string`: A sequence of text, enclosed by either double or single quotes.

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

> [!NOTE]
> Like in Python, zen uses aliasing. Basically, what that means is that doing
> something like `b = a` causes `b` and `a` to both refer to the same object in
> memory, rather than creating a new copy. So, any changes to `a` will also be
> reflected in `b`. To avoid this, use the `copy()` global function described around
> the end of this documentation.

## Exiting early

You can use the `return` statement, normally used to return from functions, to
exit a program early, similar to `exit()` functions in other languages. This takes
inspiration from Lua.

```
puts "hello"
return
puts "world"
```

This will print "hello" and exit.

You can add a number after `return` to exit with that status code.

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
the code associated with it is executed and the statement exits. 

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

Note that the `else` clause is mandatory.

A `switch true` can be used to easily emulate an `else if` statement. In fact,
by default if you don't specify any variable for the switch, the default value
is `true`.

```zen
switch {
    0 == 0 => puts "zero is zero!"
    0 == 1 => puts "zero is on- wait what?"
    else => puts "ok I don't even know anymore"
}
```

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

Additionally, if a function has a single argument which is a string, you can omit
the parentheses.

```zen
puts "hey, no parens!"
```

## Pipes

zen supports a unique feature inspired by the Elixir programming language called
pipes. Pipes allow one to pass expressions to other expressions, or pass values
to functions.

```zen
use "string"

// These two are equivalent:
print string.upcase("hello")
print "hello" |> string.upcase()
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

The list can be subscripted with a non-negative integer index to get an element out
of it.

```zen
list[0] //=> 1
```

The `push()` and `pop()` native functions in the `list` module can be used to 
add and remove items from a list, and the global `len()` native function can be used 
to get the length of the list.

> [!NOTE]
> You can also use the subscripting syntax on strings to get a character at the
> provided index.

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
with the `pub` keyword within the imported file's name.

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

This will print out "bar".

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

## Standard library

zen's standard library contains various modules and some globally available functions
for you to use.

### Functions in the global scope

- `puts(x)`: Print an expression to stdin and append a newline.
- `gets()`: Read a line from stdin.
- `len(s)`: Get the length of a string or list `s`.
- `typeof(x)`: Get the type of any expression as a string.
- `str(x)`: Convert any value into a string.
- `parse(s)`: Attempt to parse a string `s` into a floating point number.
- `copy(x)`: Return a copy of `x`.

### module `time`

- `clock()`: Get the current UNIX time in seconds.

### module `math`

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
- `write(p, m, s)`: Write the string `s` to a file `p` in the mode `m`. This function returns `nil`. If the file does not exist, it will be created. The mode is a string and may be one of the following:
    - `"w"`: To overwrite the file's contents.
    - `"a"`: To append to the file.

> [!NOTE]
> The path for the read and write functions is relative to the running program. 
> If in a REPL, the path is relative to wherever the REPL was invoked.

- `args()`: Get the arguments passed to the program as a list.

### module `string`

- `chomp(s)`: Trim whitespace from both sides of a string `s`.
- `replace(s, o, n)`: Replace all instances of a substring `o` in a string `s` with the substring `n`.
- `upcase(s)`: Turn the characters of a string `s` into uppercase.
- `downcase(s)`: Turn the characters of a string `s` into lowercase.
- `reverse(s)`: Reverse a string `s`.
- `asciichar(x)`: Convert a number to its corresponding ASCII character.
- `asciinum(s)`: Convert an ASCII character to its corresponding number.

### module `list`

- `push(l, i)`: Add an item `i` to the end of the list `l`.
- `pop(l)`: Get the last item of the list `l` after removing it from the list.

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
