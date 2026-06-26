# AGENTS.md — zen

A dynamically typed language interpreter in **Odin** with an in-progress Hindley-Milner typechecker.

## Branches

| Branch | Purpose |
|---|---|
| `main` | Stable: no type inference |
| `typechecker` | Active: HM type inference (`TYPE_CHECK :: true` at `zen/vm.odin:999`), classes & OOP removed |

## Prerequisites

- **Odin** on `PATH`, Python 3, C compiler + `ar`
- isocline auto-downloaded & compiled by `x.py`

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
| `test -n` (`--new`) | Run `test/__tests_new__/` (combined e2e + typechecker suite) |
| `bench` | Benchmarks via release build |

Always run tests through `x.py`, never `run_tests.py` directly (hardcodes `../bin/test/zen`).

## Debug flags (debug build `dzen` only)

| Flag | Effect |
|---|---|
| `--dump-tokens` | Dump tokens, exit |
| `--dump-ast` | Dump AST, exit |
| `-D, --dump` | Disassemble bytecode |
| `-T, --trace` | Trace execution |
| `-C, --compile` | Compile only (use with `-D`) |
| `-L, --log-gc` | Log GC |
| `-S, --stress-gc` | GC on every allocation |
| `--log-type` | Log type inference |

Short flags bundle: `-tD` → time + disassemble. All except `-t`/`--time` print "Unknown option" in release builds.

## Compilation pipeline

```
Lex → Parse → Semcheck → Resolve → Typecheck → Codegen → VM
```

Typechecking is skipped when `has_user_modules(expr)` is true (user modules bypass inference on `typechecker` branch; `main` branch has no typechecker at all).

## Key quirks

- **Debug-only flags**: All flags except `-t`/`--time` cause "Unknown option" in release builds.
- **Exit codes**: 65 (lex/parse/compile), 70 (runtime), 74 (read), 0 (ok).
- **NaN boxing**: `NAN_BOXING :: true` at `zen/value.odin:10` — values are `u64` using quiet-NaN mantissa bits.
- **Memory leaks**: Debug builds use `mem.Tracking_Allocator`. `--strict` makes it a failure.
- **`// DRAFT`** in a test file skips it. `// expect:` matches stdout, `// ERR:` matches stderr.
- **E2E timeout**: 2 seconds per test (infinite loops).
- **No CI, no linter config, no opencode config**.

## Testing workflow

1. `./x.py test --recompile -n` — rebuild debug binary + run new suite (typechecker branch)
2. `./x.py test --recompile` — rebuild + unit + e2e
3. `./x.py bench` — benchmark via release build
4. `./x.py dbg` → `./x.py test -u` is faster if you only changed non-Odin files
