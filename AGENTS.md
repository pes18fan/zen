# AGENTS.md

`zen` is a statically-typed scripting language (interpreter + bytecode VM)
written in Odin, with Hindley-Milner style type inference. Everything in
`zen/` is **one flat Odin package** (`package zen`, plus a vendored
`zen/isocline` binding) — there are no internal module boundaries to respect.

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
- `-vet -vet-tabs -strict-style -vet-style -warnings-as-errors -disallow-do`
  is baked into **both** debug and release flags in `x.py` — there is no
  separate lint step. Any code you add must pass strict style, have no
  warnings, and **never use `do`** (`do ...` is disallowed).
- Release uses `-o:aggressive`; a comment in `x.py` notes to switch to
  `-o:speed` if that shows weird codegen behavior.
- `CHAOTIC=true` unlocks joke features (`ifn't`, `whilen't`) that are
  intentionally untested — don't expect test coverage for them.
- Debug binary is named `dzen` (not `zen`).
- The debug binary provides some flags to observe the behavior of the interpreter,
    like dumping the AST, dumping bytecode, tracing VM execution et cetera.
    View available flags with `dzen -h`. These flags do not exist in the release
    build.

## Testing

```bash
./x.py test                  # unit tests + e2e suite (test/__tests__)
./x.py test --recompile      # rebuild debug first, copy to bin/test/zen, test
./x.py test --unit   -u      # odin unit tests only (@(test) procs)
./x.py test --e2e    -e      # e2e .zn script tests only
./x.py test --strict -s      # fail on memory leaks (debug build only)
```

- **The e2e runner needs a prebuilt interpreter at `bin/test/zen`.** Created
  by `test --recompile` (builds debug, copies `bin/dbg/dzen` →
  `bin/test/zen`). Without it, `run_tests.py` exits with "interpreter not
  found". When in doubt, run `./x.py test --recompile`.
- **There is a single e2e suite at `test/__tests__`.** It covers the whole
  language, including a `typechecking/` subdir for type-error and inference
  tests. (The old legacy suite and `test/__tests_new__` were consolidated
  when classes & OOP were removed from the language.)
- The typechecker can be compiled out with `-define:DISABLE_TYPECHECKER=true`
  (default is on; see `zen/vm.odin:13`). The `--log-checker` debug flag logs
  the typechecker's passes.
- No built-in single-file flag in `run_tests.py`, but you can point it at a
  subfolder via `python test/run_tests.py -d <relpath>` (run from `test/`).
  To run one file by hand: build, then `bin/test/zen path/to/file.zn` and
  compare to `// expect:`/`// ERR:` comments yourself.
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

## Current limitations

- Type inference is **skipped for programs that `use` a `.USER` module**:
  `has_user_modules` at `zen/semcheck.odin:341`, called at `zen/vm.odin:956`;
  typechecker runs only when `!should_not_typecheck` (`vm.odin:967`). The
  blocker (per TODO at `vm.odin:958-966`) is that the typechecker cannot
  cross module boundaries — resolver and checker don't mesh across them yet.
- Information from the typechecker, resolver, and module resolver is **not
  consumed by codegen or the VM**. Codegen re-resolves variables clox-style
  and the VM re-parses module imports at runtime. This is redundant but not
  incorrect; slated for a future refactor.
