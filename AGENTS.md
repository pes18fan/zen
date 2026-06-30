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
| `typechecker` | Active: HM type inference (`TYPE_CHECK :: true` at `zen/vm.odin:1042`), classes & OOP fully removed from language |

## Build

All dev work goes through `./x.py` at the repo root (run it from there).
Requires Odin and Python (stdlib only, no pip deps).

```bash
./x.py dbg        # debug build -> bin/dbg/dzen (dzen.exe on Windows)
./x.py rel        # release build -> bin/rel/zen
./x.py chaotic    # build with -define:CHAOTIC=true -> bin/chaotic/zen
./x.py clean      # rm -rf bin/
./x.py run        # run debug build (pass --args to add CLI flags)
./x.py run --args "file.zn"   # example
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
- Debug binary is named `dzen` (not `zen`); the README's `bin/dbg/zen` is wrong.

## Testing

```bash
./x.py test                  # unit tests + old e2e suite (test/__tests__)
./x.py test --recompile      # rebuild debug first, copy to bin/test/zen, test
./x.py test --unit   -u      # odin unit tests only (@(test) procs)
./x.py test --e2e    -e      # e2e .zn script tests only
./x.py test --new    -n      # run test/__tests_new__ instead (recommended)
./x.py test --strict -s      # fail on memory leaks (debug build only)
```

- **The e2e runner needs a prebuilt interpreter at `bin/test/zen`.** Created
  by `test --recompile` (builds debug, copies `bin/dbg/dzen` →
  `bin/test/zen`). Without it, `run_tests.py` exits with "interpreter not
  found". When in doubt, run `./x.py test --recompile`.
- **Two e2e suites exist and they are not the same thing:**
  - `test/__tests__` (default) — legacy suite with OOP tests
    (class/inheritance/super/this) that are **expected to fail** since
    classes are fully removed from the language.
  - `test/__tests_new__` (`--new`) — the active suite for the ongoing
    typechecker work; has a `typechecking/` subdir and no OOP tests.
    **This is the working suite. Use `--new`.**
- To run a single `.zn` test file directly: build, then
  `bin/test/zen path/to/file.zn` and compare to `// expect:`/`// ERR:`
  comments by hand. No built-in single-file flag in `run_tests.py`.
- Test file format: `// expect: <line>` = expected stdout (line-for-line
  match); `// ERR: <text>` = exit must be non-zero and **only first stderr
  line** is checked (substring). `// DRAFT` anywhere in the file skips it.
- Each e2e test has a hard 2-second timeout (catches infinite loops).
- Odin unit tests live beside their subject as `zen/<name>_test.odin`
  (e.g. `chunk_test.odin`, `lexer_test.odin`, `type_checker_test.odin`),
  using `@(test)` procs from `core:testing`. No per-test filter wired up.

## Benchmarks / docs

```bash
./x.py bench               # runs test/benchmark/*.zn against bin/rel/zen
./x.py bench --recompile   # rebuild release first
./x.py doc                 # odin doc zen -> doc/docs.txt
```
Man page: `pandoc -s -t man ./etc/zen.1.md -o zen.1`

## Code conventions worth knowing

- Error propagation uses the `try`/`try2` proc-group helpers in
  `zen/error.odin` (overloaded per pass: codegen/semantic/resolver) instead
  of repeating `if err != nil { ... }`. Use these rather than hand-rolling
  error checks in those passes.
- `parse_type_annotation` in `zen/parser.odin` allocates `Type` type-args on
  the **general-purpose AST allocator**, not the type checker's arena — any
  code freeing AST nodes must also free these. This is the one spot where
  the arena-everywhere assumption for types doesn't hold.

## Current limitations (typechecker branch)

- Type inference is **gated on user-defined modules**: if a program uses
  `use` with a `.USER` module, the typechecker is skipped entirely
  (`has_user_modules` proc at `zen/semcheck.odin:350`, called at
  `zen/vm.odin:1038-1046` — forced by the lack of a module resolution pass
  after parsing). This is the next thing to fix.
