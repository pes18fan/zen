# AGENTS.md — Zen Programming Language

## Project Overview

**Zen** is a lightweight dynamically typed programming language implemented in **Odin**. It features bytecode interpretation, garbage collection, OOP with classes and inheritance, first-class functions with closures, Elixir-inspired pipes (`|>`), and a file-based module system. 
Files use the `.zn` extension. Version `0.0.1`, authored by **pes18fan**, licensed under MIT.

## Repository Structure

```
zen/
├── zen/                  # Main Odin source (package zen)
│   ├── main.odin         # Entry point, CLI, REPL
│   ├── lexer.odin        # Tokenizer with ASI
│   ├── parser.odin       # Pratt parser → AST
│   ├── compiler.odin     # Bytecode compiler (AST → bytecode)
│   ├── vm.odin           # Bytecode interpreter
│   ├── value.odin        # Value representation (NaN boxing)
│   ├── object.odin       # Heap objects (ObjFunction, ObjString, ObjClosure, etc.)
│   ├── chunk.odin        # Bytecode chunk + OpCode enum
│   ├── table.odin        # Hash table (open addressing)
│   ├── gc.odin           # Mark-sweep garbage collector
│   ├── std.odin          # Standard library (time, math, os, string, list)
│   ├── error.odin        # try/try2 error helpers
│   ├── format.odin       # Terminal color helpers (red, green, yellow)
│   ├── debug.odin        # Disassembly/debug
│   ├── type_checker.odin # Hindley-Milner type checker (WIP, Algorithm W)
│   ├── type_checker_test.odin
│   ├── chunk_test.odin
│   ├── lexer_test.odin
│   └── isocline/         # Vendored REPL library bindings
├── test/
│   ├── __tests__/        # End-to-end .zn tests (organized by feature)
│   ├── benchmark/        # Performance benchmarks (.zn files)
│   ├── run_tests.py      # E2E test runner
│   └── run_benchmarks.py # Benchmark runner
├── examples/             # Example .zn programs
├── syntaxes/             # Editor syntax highlighting (VSCode, Sublime, Vim)
├── x.py                  # Python build script (278 lines)
├── DOCUMENTATION.md      # Full language reference (558 lines)
├── .zen_version          # Contains "0.0.1"
├── .gitignore            # Ignores /.vscode/, /bin/, /doc/, /ols.json, /isocline
└── LICENSE               # MIT
```

## Build System (`x.py`)

Requires **Odin compiler** and **Python**. The `isocline` REPL library is auto-downloaded and compiled by the build script.

| Command | Description |
|---|---|
| `./x.py dbg` | Debug build → `./bin/dbg/dzen` |
| `./x.py rel` | Release build → `./bin/rel/zen` (also copied to `./bin/test/zen`) |
| `./x.py chaotic` | Chaotic build → `./bin/chaotic/zen` (`-define:CHAOTIC=true`) |
| `./x.py test` | Unit tests + end-to-end tests, recompile test interpreter via `--recompile` flag |
| `./x.py bench` | Run benchmarks |
| `./x.py clean` | Remove build artifacts |
| `./x.py doc` | Generate docs at `doc/docs.txt` |
| `./x.py run --args "..."` | Run debug build with args |

**Odin flags**: Debug uses `-vet -debug`, Release uses `-vet -o:speed`.

## Testing

### Unit Tests (Odin)

- Files: `lexer_test.odin`, `chunk_test.odin`, `type_checker_test.odin`
- Run via: `odin test zen` (integrated into `./x.py test`)

### End-to-End Tests (`test/__tests__/`)

- `.zn` files with special comment annotations:
  - `// expect: <output>` — expected stdout (line-by-line, whitespace-stripped comparison)
  - `// ERR: <message>` — expected error message substring
  - `// DRAFT` — marks test as draft (skipped)
- Runner: `test/run_tests.py`, interpreter: `../bin/test/zen`, timeout: 2s
- Categories: `assignment/`, `class/`, `closure/`, `comments/`, `conditionals/`,
    `constructor/`, `equality/`, `field/`, `for/`, `for_in/`, `function/`,
    `inheritance/`, `list/`, `loop_control/`, `math/`, `method/`, `modules/`,
    `native/`, `operators/`, `pipes/`, `string/`, `super/`, `switch/`,
    `syntax_error/`, `this/`, `variable/`, `while/`

### Benchmarks

- Run via: `python test/run_benchmarks.py`

## Architectural Pipeline

```
Source string → Lexer → Token stream (with ASI) → Parser → AST (Expr-based, no Stmt/Decl nodes) → Compiler → Bytecode (Chunk) → VM (interpreter) + GC
```

Everything in zen is an **expression**. There are no statement or declaration AST
node types — `if`, `while`, `for`, `switch`, `var`/`val`, `class`, `use`, `func`,
`print`, `return`, `exit`, `break`, `continue` are all parsed as expression nodes
(`IfExpr`, `WhileExpr`, etc.). Named function declarations (`func name() {}`)
are syntactic sugar for `var name = func() {}`.

Expression chaining at the top level uses `SequenceExpr` (left/right chain),
with `;` and NEWLINE acting as expression separators.

## Odin Coding Conventions

### Naming
- **Types** (structs, enums): PascalCase (`TokenType`, `Lexer`, `ObjString`, `CallFrame`, `ValueArray`)
- **Procedures**: snake_case (`init_lexer`, `lex_token`, `emit_byte`, `vm_push`, `free_chunk`)
- **Variables**: snake_case (`source`, `current`, `line`, `loop_start`)
- **Enum variants**: SCREAMING_SNAKE_CASE (`.OP_CONSTANT`, `.INTERPRET_OK`)
- **File names**: snake_case (`type_checker.odin`, `lexer_test.odin`)

### Imports

```odin
import "core:fmt"
import "core:os"
import "core:strings"
import "core:path/filepath"
import ic "isocline"
```

### Comments
- Line `//` and block `/* */` comments. Block comments used for documentation above procedures and structs.

### Attributes / Decorators

- `@(private = "file")` — file-private procedure
- `@(require_results)` — must use return value
- `#force_inline` — force inlining
- `#no_bounds_check` — skip bounds checking
- `@(optimization_mode = "favor_size")` — optimize for size

### Conditional Compilation

```odin
when CHAOTIC { ... }
when ODIN_DEBUG { ... }
when NAN_BOXING { ... }
when TYPE_CHECK { ... }
```

### Compile-Time Config

```odin
#config(CHAOTIC, false)       // Build-time config flag
#load("../.zen_version")       // File include at compile time
```

### Error Handling Pattern

```odin
try(cg, some_proc()) or_return
try2(cg, some_proc_returning_value()) or_return
```

- `ErrorMessage` is `Maybe(string)` (defined in `error.odin`)
- Error handling with `try`/`try2` polymorphic procedures that accept either `^Codegen` or `^TypeChecker`

### Memory Management

- Manual memory with `make`, `delete`, `free`, `new`
- GC manages heap objects (Obj* types)
- Explicit arena allocator: `context.temp_allocator`
- `defer` for cleanup
- `temp_push`/`temp_pop` for GC root protection during allocations

### Key Odin Idioms
- Pointer params: `^Type` (e.g., `^Lexer`, `^VM`, `^GC`)
- Struct embedding: `using obj: Obj`
- Tagged unions: `Value :: union { bool, f64, ^Obj }` (fallback; NaN boxing is default)
- `or_return` for early error propagation
- `#partial switch` for non-exhaustive switches
- `fmt.tprintf`, `fmt.ctprintf` for formatted strings
- `cast(^Type)(ptr)` for pointer casting
- `fmt.eprintf` / `color_red(os.stderr, ...)` for error output

### Value Representation
- **NaN boxing** (default): `Value :: u64` — numbers are raw f64, others are tagged NaN pointers
- **Tagged union** fallback: `Value :: union { bool, f64, ^Obj }`
- Predicate/accessor pattern: `is_bool(v)`, `as_bool(v)`, `bool_val(true)`

## Zen Language Rules (for writing `.zn` files)

### Syntax

- **Expressions**: separated by newlines (ASI) or semicolons. ASI does not insert within lists, inside parentheses, or when a line continues an expression.
- **Comments**: `//` line comments
- **Variables**: `var` (mutable), `val` (single-assign/final). Multiple declarations supported: `var a = 1, b = 2, c`. Multi-line: `var\n    a = 1,\n    b = 2`. Uninitialized vars default to `nil`.
- **Types**: `number` (f64), `bool`, `string` (immutable), `nil`, `list` (mutable, growable)
- **Functions**: `func name(params) { body }`, arrow `func name(params) => expr`, anonymous `func(params) { body }`
- **Classes**: `class Name { init() { ... } method() { ... } }`, single inheritance with `class Child < Parent`
- **Conditionals**: `if/else` (no else-if — use `switch`), `switch { pattern => body; else => body }`
- **Loops**: `while`, `for` (C-style: `for var i=0; i<n; i=i+1`), `for in` over lists/strings
- **Loop control**: `break`, `continue`
- **Blocks**: `{ ... }` creates a new scope and returns the last expression's value
- **Pipes**: `expr |> func()` — passes `expr` as first arg to `func`. `it` keyword references the piped value.
- **Modules**: `use "module"` for builtins or `use "./path.zn"` for files. `pub` keyword exports from file modules.
- **Built-in functions**: `puts()`, `gets()`, `len()`, `typeof()`, `str()`, `parse()`, `copy()`, `dirname()`, `filename()`
- **Standard library modules**: `time`, `math`, `os`, `string`, `list`
- **Chaotic mode**: `ifn't` / `whilen't` available when compiled with `CHAOTIC=true`
- **Error/exit**: `exit` expression with optional status code
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`, `and`, `or`
- **Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=`
- **String operations**: `+` for concatenation, `[]` for indexing
- **List operations**: `[]` for indexing/setting, `list.*` module functions for mutation
- **Function calls**: parentheses optional for single string argument

### Semantic Rules

- Functions can be called without parens when passing a single string: `puts "hello"`
- No else-if; use switch instead
- Switch requires an `else` clause (no fallthrough — each arm is independent)
- `val` variables cannot be reassigned (but their properties/items can be mutated — only the binding is final)
- `return` outside a function is a runtime error
- Break/continue only valid inside loops
- `this` in a method refers to the receiver instance
- `super` for accessing parent class methods
- Classes use copy-down inheritance (parent methods copied to child at declaration)
- Function arity is checked at call time
- Uninitialized variables default to `nil`
- Modules are singletons — only parsed and executed once per import path
- `it` keyword is only valid inside a pipeline expression
- Named function declarations (`func name() {}`) are syntactic sugar over `var name = func() {}`
- Global function declarations and named lambdas are hoisted by `collect_expr_globals` before codegen (enables mutual recursion)

## CLI Flags

| Flag | Description |
|---|---|
| `-c` | Compile only (don't execute) |
| `-d` | Dump bytecode disassembly |
| `-t` | Dump tokens |
| `-a` | Dump AST |
| `-T` | Trace execution |
| `-G` | Stress garbage collector (GC on every allocation) |
| `-L` | Log type checker |
| `-l` | Log GC |
| `-r` | Record execution time |
| `-v` | Print version |

## Git Workflow

- Remote: `origin` → `https://github.com/pes18fan/zen`
- Branches: `main`
- Branch protection: not enforced; any branch can be pushed to

## When Adding Tests

1. Place `.zn` files in `test/__tests__/<category>/`
2. Add `// expect: <expected output>` for success cases
3. Add `// ERR: <expected error>` for error cases
4. Add `// DRAFT` to skip a test
5. Run with `./x.py test`

## When Adding Features

1. The pipeline is: Lexer → Parser → Compiler → VM (+ GC)
2. Add new tokens to the lexer if needed, new AST nodes to the parser, new opcodes to `chunk.odin`'s `OpCode` enum, codegen in compiler, execution in VM
3. Update `debug.odin` for disassembly of new opcodes
4. Add standard library functions in `std.odin` if applicable
5. Add e2e tests in `test/__tests__/`
6. Update `DOCUMENTATION.md` if the feature changes the language surface
7. Add syntax highlighting to `syntaxes/` for VSCode, Sublime, and Vim
