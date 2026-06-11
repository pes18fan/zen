# AGENTS.md — zen

A dynamically typed language interpreter, written in **Odin** (not Rust, C, or Go). Source: `zen/*.odin`. Entrypoint: `zen/main.odin`.

## Prerequisites

- [Odin](https://odin-lang.org) compiler (must be on `PATH`)
- Python 3 (for `x.py` build script, test runner, benchmarks)
- C compiler + `ar` (isocline is auto-downloaded and compiled by `x.py`)

## Build commands (all via `./x.py`)

| Command | Result |
|---|---|
| `./x.py dbg` | Debug build → `bin/dbg/dzen` |
| `./x.py rel` | Release build → `bin/rel/zen` |
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

- **Unit tests**: `odin test zen` — standard Odin `@(test)` procs in `*_test.odin` files.
- **E2E tests**: `test/run_tests.py` — runs `.zn` files via `bin/test/zen`. Compare expected output with `// expect:` comment lines. Expect an error with `// ERR:`. Mark as draft (skipped) with `// DRAFT`.
- **Typechecking tests**: `test/run_tests.py -d typechecking/` — same format.
- **Benchmarks**: `test/run_benchmarks.py` — runs `.zn` files via `bin/rel/zen`, expects last stdout line to be a time in ms.

## Project structure

```
zen/           — compiler/interpreter source (Odin package)
core/          — builtin.zn (LSP type stub, not compiled)
test/          — e2e tests in __tests__/, typechecking tests, benchmarks
examples/      — example .zn programs
syntaxes/      — VSCode/Sublime/vim syntax highlighting
etc/           — man page template
doc/           — generated docs
x.py           — build/test/bench entrypoint
DOCUMENTATION.md — language reference (651 lines)
```

## Important quirks

- **Debug-only flags**: `--dump-tokens`, `--dump-ast`, `-D` (disassemble), `-T` (trace), `-L` (log GC), `-S` (stress GC) are **only available in debug builds**. Release builds ignore them and print "Unknown option".
- **Memory leak detection**: Debug builds use Odin's `mem.Tracking_Allocator`. Use `--strict` on `x.py test` or `run_tests.py` to fail on leaks.
- **E2E test interpreter**: always uses `bin/test/zen` (a copy of the debug build). Rebuild with `--recompile` to update it.
- **Benchmark interpreter**: uses `bin/rel/zen`. Rebuild with `--recompile` or `./x.py rel` first.
- **Chaotic build**: `./x.py chaotic` — release build with `-define:CHAOTIC=true` for experimental features.
- **`// DRAFT`**: Tests with this marker are completely skipped by both e2e and benchmark runners.
- **No CI/CD**: No `.github/`, no pre-commit hooks, no task runners.
- **No package manager**: No `Cargo.toml`, `package.json` (except VSCode extension in `syntaxes/vscode/`). Just Odin + Python.
