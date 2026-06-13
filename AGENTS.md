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

- **Unit tests**: `odin test zen` — `@(test)` procs in `*_test.odin`.
- **E2E tests**: `test/run_tests.py` — runs `.zn` files via `bin/test/zen`. Compare output with `// expect:` comments, expect error with `// ERR:`, skip with `// DRAFT` (case-sensitive).
- **Typechecking tests**: `test/run_tests.py -d typechecking/` — same format.
- **Benchmarks**: `test/run_benchmarks.py` — runs `.zn` via `bin/rel/zen`, expects last stdout line as time in ms.

## Project structure

| Path | Purpose |
|---|---|
| `zen/*.odin` | Compiler/interpreter source (~12.9k LoD) |
| `core/builtin.zn` | LSP type stubs (not compiled) |
| `test/__tests__/` | E2E test `.zn` files |
| `test/typechecking/` | Typechecker test `.zn` files |
| `test/benchmark/` | Benchmark `.zn` files |
| `examples/` | Example `.zn` programs |
| `syntaxes/` | VSCode/Sublime/vim syntax highlighting |
| `DOCUMENTATION.md` | Language reference (651 lines) |

## Key quirks

- **Debug-only flags**: `--dump-tokens`, `--dump-ast`, `-D` (disassemble), `-T` (trace), `-L` (log GC), `-S` (stress GC) are only in debug builds. Release builds print "Unknown option".
- **CLI**: Short flags bundle (e.g. `-tD` for time + disassemble). Run `zen -h` to see all.
- **Exit codes**: 65 (lex/parse/compile error), 70 (runtime error), 74 (read error), 0 (ok).
- **NaN boxing**: `NAN_BOXING :: true` in `value.odin` — values are `u64` using quiet-NaN mantissa bits.
- **Memory leaks**: Debug builds use `mem.Tracking_Allocator`. Pass `--strict` to fail on leaks.
- **Test runner**: Run through `x.py` instead of `run_tests.py` directly (`run_tests.py` hardcodes paths).
- **E2E interpreter**: `bin/test/zen` (a copy of debug build). Rebuild with `--recompile`.
- **Benchmark interpreter**: `bin/rel/zen`. Rebuild with `--recompile` or `./x.py rel`.
- **`// DRAFT`**: Tests with this marker are skipped by both e2e and benchmark runners.
- **`typechecker` branch**: Active development; separates typechecking tests from main e2e suite temporarily.
