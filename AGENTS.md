# AGENTS.md — zen

A dynamically typed language interpreter in **Odin** with an in-progress Hindley-Milner typechecker (`typechecker` branch).

## Prerequisites

- **Odin** compiler on `PATH` (checked by `x.py` via `odin version`)
- Python 3 (for `x.py`, test runner, benchmarks)
- C compiler + `ar` (isocline auto-downloaded & compiled by `x.py`; git clone into `isocline/`)

## Build commands (all via `./x.py`)

| Command | Result |
|---|---|
| `./x.py dbg` | Debug build → `bin/dbg/dzen` (flags: `-vet -debug`) |
| `./x.py rel` | Release build → `bin/rel/zen` (flags: `-vet -o:aggressive`) |
| `./x.py chaotic` | Release build with `-define:CHAOTIC=true` |
| `./x.py clean` | Removes `bin/` |
| `./x.py doc` | Generates `doc/docs.txt` via `odin doc` |
| `./x.py run --args "file.zn"` | Runs a file with debug build |

## Test commands

```bash
./x.py test              # unit tests + e2e tests (default)
./x.py test --recompile  # rebuild debug binary first
./x.py test --strict -s  # fail on memory leaks (debug build only)
./x.py test -u           # unit tests only
./x.py test -e           # e2e tests only
./x.py test -t           # typechecking tests only
./x.py bench             # benchmarks (release build)
```

**Critical**: Always run tests through `x.py`, never `run_tests.py` directly.

## Debug flags

Flags provided by the debug binary `dzen`, useful when hunting down errors.

| Flag | Result |
|---|---|
| `-C, --compile` | Compile only, useful with `-D` |
| `-D, --dump` | Dump disassembled bytecode |
| `-T, --trace` | Trace script execution |
| `--dump-tokens` | Dump tokens from the lexer and exit |
| `--dump-ast` | Dump the AST produced by the parser and exit |
| `-L, --log-gc` | Log garbage collection |
| `-S, --stress-gc` | Run the GC on every allocation |
| `--log-type` | Log the type checker |

## Project structure

| Path | Purpose |
|---|---|
| `zen/main.odin` | Entry point, argument parsing, REPL |
| `zen/vm.odin` | Virtual machine, bytecode interpreter |
| `zen/type_checker.odin` | Hindley-Milner type checker |
| `zen/*.odin` (excluding tests) | Compiler/interpreter core |
| `zen/*_test.odin` | Unit tests (run via `odin test zen`) |
| `test/__tests__/` | E2E tests (run via `x.py test`) |
| `test/typechecking/` | Typechecking tests (run via `x.py test -t`) |
| `test/benchmark/` | Benchmarks (run via `x.py bench`) |
| `examples/` | Example programs |
| `core/builtin.zn` | LSP type stubs |

## Key quirks

- **Debug-only flags**: `--dump-tokens`, `--dump-ast`, `-D` (disassemble), `-T` (trace), `-L` (log GC), `-S` (stress GC) are only in debug builds. Release builds print "Unknown option".
- **CLI**: Short flags bundle (e.g. `-tD` for time + disassemble). Run `zen -h` to see all.
- **Exit codes**: 65 (lex/parse/compile error), 70 (runtime error), 74 (read error), 0 (ok).
- **NaN boxing**: `NAN_BOXING :: true` in `value.odin:10` — values are `u64` using quiet-NaN mantissa bits.
- **Memory leaks**: Debug builds use `mem.Tracking_Allocator`. Pass `--strict` to fail on leaks.
- **Test runner**: Run through `x.py` instead of `run_tests.py` directly (`run_tests.py` hardcodes paths).
- **E2E interpreter**: `bin/test/zen` (a copy of debug build). Rebuild with `--recompile`.
- **Benchmark interpreter**: `bin/rel/zen`. Rebuild with `--recompile` or `./x.py rel`.
- **`// DRAFT`**: Tests with this marker are skipped by both e2e and benchmark runners.
- **`typechecker` branch**: Active development; separates typechecking tests from main e2e suite temporarily.

## Testing workflow

1. **Unit tests**: Run `odin test zen` (or `./x.py test -u`)
2. **E2E tests**: Run `./x.py test -e` (requires `bin/test/zen`)
3. **Typechecking tests**: Run `./x.py test -t` (requires `bin/test/zen`)
4. **Benchmarks**: Run `./x.py bench` (requires `bin/rel/zen`)

**Important**: When running multiple test types, use `./x.py test --recompile` first to rebuild the debug binary.

## Architecture notes

- **Two-phase compilation**: Source → AST → Resolution → Typechecking → Codegen → Bytecode → Interpretation
- **REPL**: Built-in REPL with persistent type checker state
- **Module system**: File-based with `pub` keyword for public exports
- **Type system**: Hindley-Milner with type inference, currently in `typechecker` branch
- **Value representation**: NaN boxing for compact value storage
- **Memory management**: Custom garbage collector with arena allocation for type checking

## Common pitfalls

- Debug-only flags will cause "Unknown option" errors in release builds
- Test files use `// expect:` for expected output and `// ERR:` for expected errors
- E2E tests timeout after 2 seconds (infinite loops)
- Typechecking tests are separate from e2e tests (different directory)
- Release builds have `-o:aggressive` optimization, debug builds have `-vet -debug`
- The `typechecker` branch is active; typechecking tests are isolated
