# AGENTS.md

`zen` is a dynamically-typed scripting language (interpreter + bytecode VM)
written in Odin, currently mid-transition to a Hindley-Milner static type
system. Everything in `zen/` is **one flat Odin package** (`package zen`,
plus a vendored `zen/isocline` binding) — there are no internal module
boundaries to respect.

## Branches

| Branch | Purpose |
|---|---|
| `main` | Stable: no type inference |
| `typechecker` | Active: HM type inference (`TYPE_CHECK :: true` at `zen/vm.odin:1003`), classes & OOP removed |

## Build

All dev work goes through `./x.py` at the repo root (run it from there).
Requires Odin and Python (stdlib only, no pip deps).

```bash
./x.py dbg        # debug build -> bin/dbg/dzen (dzen.exe on Windows)
./x.py rel        # release build -> bin/rel/zen
./x.py chaotic    # build with -define:CHAOTIC=true -> bin/chaotic/zen
./x.py clean      # rm -rf bin/
```

- First build auto-clones+compiles `isocline` (the REPL line-editing lib)
  into `./isocline/`; this needs a C compiler and network access to
  `github.com/daanx/isocline`, and only happens once (cached after).
- `-vet` is always on (baked into `DEBUG_FLAGS`/`RELEASE_FLAGS` in `x.py`) —
  there is no separate lint step/command.
- Release uses `-o:aggressive`; a comment in `x.py` notes to switch to
  `-o:speed` if that shows weird codegen behavior.
- `CHAOTIC=true` unlocks joke features (`ifn't`, `whilen't`) that are
  intentionally untested — don't expect test coverage for them.

## Testing

```bash
./x.py test                  # unit tests (odin test) + old e2e suite (test/__tests__)
./x.py test --recompile      # rebuild debug binary first, copy to bin/test/zen, then test
./x.py test --unit   -u      # odin unit tests only (the @(test) procs in zen/*_test.odin)
./x.py test --e2e    -e      # e2e .zn script tests only
./x.py test --new    -n      # run test/__tests_new__ instead of test/__tests__
./x.py test --strict -s      # also fail on memory leaks (debug build only)
```

- **The e2e runner needs a prebuilt interpreter at `bin/test/zen`.** It is
  only created by `test --recompile` (which builds debug and copies
  `bin/dbg/dzen` → `bin/test/zen`). Without `--recompile` and no prior build,
  `run_tests.py` exits immediately with "interpreter not found". When in
  doubt, run `./x.py test --recompile`.
- **Two e2e suites exist and they are not the same thing:**
  - `test/__tests__` (default) — the legacy suite, includes OOP
    (class/inheritance/super/this) tests.
  - `test/__tests_new__` (`--new`) — the active suite for the ongoing
    typechecker + OOP-removal work; has a `typechecking/` subdir and no
    class/inheritance/super/this tests. If you're working on the type
    checker, run with `--new`.
- To run a single `.zn` test file directly (skip the harness): build, then
  `bin/test/zen path/to/file.zn` and compare to its `// expect:`/`// ERR:`
  comments by hand. There's no built-in single-file/single-package flag in
  `run_tests.py`.
- Test file format (`test/run_tests.py`): `// expect: <line>` comments give
  expected stdout (matched line-for-line); `// ERR: <text>` means the program
  should exit non-zero and **only the first line of stderr** is checked
  (substring match, not exact). A `// DRAFT` comment anywhere in the file
  skips it entirely (listed as "not run", not as a pass/fail).
- Each e2e test has a hard 2-second timeout (treated as a failure, used to
  catch infinite loops) — don't write `.zn` tests that legitimately need
  longer.
- Odin unit tests live beside their subject as `zen/<name>_test.odin`
  (e.g. `chunk_test.odin`, `lexer_test.odin`, `type_checker_test.odin`),
  using `@(test)` procs from `core:testing`. `odin test zen` (what `x.py`
  calls) runs all of them; there's no per-test filter wired up here.

## Benchmarks / docs

```bash
./x.py bench               # runs test/benchmark/*.zn against bin/rel/zen
./x.py bench --recompile   # rebuild release first
./x.py doc                 # odin doc zen -> doc/docs.txt
```
Man page: `pandoc -s -t man ./etc/zen.1.md -o zen.1` (per README).

## Code conventions worth knowing

- Error propagation uses the `try`/`try2` proc-group helpers in
  `zen/error.odin` (overloaded per pass: codegen/semantic/resolver) instead
  of repeating `if err != nil { ... }`. Use these rather than hand-rolling
  error checks in those passes.
- `parse_type_annotation` in `zen/parser.odin` allocates `Type` type-args on
  the **general-purpose AST allocator**, not the type checker's arena — any
  code freeing AST nodes must also free these, per the `NOTE:` comment above
  that function. This is the one spot where the arena-everywhere assumption
  for types doesn't hold.
