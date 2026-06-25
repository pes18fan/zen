# AGENTS.md — zen

A dynamically typed language interpreter in **Odin** with an in-progress Hindley-Milner typechecker (active `typechecker` branch).

## Prerequisites

- **Odin** on `PATH` (checked by `x.py`)
- Python 3
- C compiler + `ar` (isocline auto-downloaded & compiled by `x.py`)

## Build commands

| `./x.py ...` | Result |
|---|---|
| `dbg` | Debug build → `bin/dbg/dzen` (`-vet -debug`) |
| `rel` | Release build → `bin/rel/zen` (`-vet -o:aggressive`) |
| `chaotic` | Release build + `-define:CHAOTIC=true` |
| `run --args "file.zn"` | Run file with debug build |
| `doc` | Generate `doc/docs.txt` |
| `clean` | Remove `bin/` |

## Test commands

| `./x.py ...` | Action |
|---|---|
| `test` | Unit tests + e2e tests |
| `test --recompile` | Rebuild debug binary first |
| `test -s` (`--strict`) | Fail on memory leaks (debug build only) |
| `test -u` | Unit tests only |
| `test -e` | E2E tests only (directory `test/__tests__/`) |
| `test -n` (`--new`) | Run `test/__tests_new__/` (typechecker + non-OOP suite) |
| `bench` | Benchmarks via release build |

Always run tests through `x.py`, never `run_tests.py` directly (hardcodes paths).

## Debug flags (debug build `dzen` only)

| Flag | Effect |
|---|---|
| `--dump-tokens` | Dump tokens from lexer, exit |
| `--dump-ast` | Dump AST from parser, exit |
| `-D, --dump` | Disassemble bytecode |
| `-T, --trace` | Trace execution |
| `-C, --compile` | Compile only (use with `-D`) |
| `-L, --log-gc` | Log GC |
| `-S, --stress-gc` | GC on every allocation |
| `--log-type` | Log type inference |

Short flags bundle: `-tD` → time + disassemble. These all print "Unknown option" in release builds.

## Project structure

| Path | Role |
|---|---|
| `zen/main.odin` | Entry point, CLI, REPL |
| `zen/vm.odin` | VM + `interpret()` — orchestrates the full pipeline |
| `zen/lexer.odin` | Lexer |
| `zen/parser.odin` | Parser |
| `zen/semcheck.odin` | Semantic analysis pass |
| `zen/resolver.odin` | Variable resolution (scopes, upvalues) |
| `zen/type_checker.odin` | Hindley-Milner type inference |
| `zen/compiler.odin` | Bytecode codegen |
| `zen/*_test.odin` | Unit tests (`odin test zen`) |
| `test/__tests__/` | E2E tests |
| `test/__tests_new__/` | New suite: replaces `__tests__` + old `typechecking/` dirs |
| `test/benchmark/` | Benchmarks |
| `examples/` | Example `.zn` programs |
| `core/builtin.zn` | LSP type stubs (future use) |

## Compilation pipeline

```
Lex → Parse → Semcheck → Resolve → Typecheck → Codegen → VM
```

`typecheck` is conditional: disabled via `TYPE_CHECK :: true` + OOP guard at `vm.odin:999` — if the program uses classes/`super`/`this` or user modules, typechecking is skipped entirely and the program runs dynamically.

## Key quirks

- **Debug-only flags**: All flags except `-t`/`--time` cause "Unknown option" in release builds.
- **Exit codes**: 65 (lex/parse/compile), 70 (runtime), 74 (read), 0 (ok).
- **NaN boxing**: `NAN_BOXING :: true` at `value.odin:10` — values are `u64` using quiet-NaN mantissa bits.
- **Memory leaks**: Debug builds use `mem.Tracking_Allocator`. `--strict` makes it a failure.
- **`// DRAFT`** in a test file skips it. `// expect:` matches stdout, `// ERR:` matches stderr.
- **`typechecker` branch**: Active development. `test/__tests_new__/` is the new suite; old `test/typechecking/` is removed.
- **`test -n`** (`--new`): runs `test/__tests_new__/` — a combined non-OOP e2e + typechecking suite.
- **E2E timeout**: 2 seconds per test (infinite loops).
- **CI / config files**: None exist (no `.github`, no opencode config, no linter config).

## Testing workflow

1. `./x.py test --recompile` — rebuild `bin/test/zen`, run unit + e2e
2. `./x.py test --recompile -n` — rebuild, run the new suite
3. `./x.py bench` — benchmark via release build

`./x.py dbg` → `./x.py test -u` is faster than `--recompile` if you only changed non-Odin files.
