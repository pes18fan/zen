# AGENTS.md

`zen` is a dynamically-typed scripting language (interpreter + bytecode VM)
written in Odin. Everything in `zen/` is **one flat Odin package**
(`package zen`), plus two packages that are **not part of the build**: the
vendored `zen/isocline` binding and the reference `zen/typechecker` package
(uncompiled dead code from the abandoned Hindley-Milner era, kept for later
revival). There are no other internal module boundaries to respect.

Current language state: fully dynamic again — classes & OOP are removed, and
modules are first-class values accessed with `.` (`math.ln(2)`,
`string.upcase("hello")`; the old `\` access is gone). Internals were renamed
accordingly: `ObjModule` → `ObjRecord`, `ModuleAccessExpr` → `GetExpr`.

The language reference is `DOCUMENTATION.md` at the repo root — read it before
changing language semantics. `doc/docs.txt` is generated Odin API docs
(`./x.py doc`), not hand-maintained.

## Branches

| Branch | Purpose |
|---|---|
| `main` | Stable: no type inference |
| `typechecker` | Active: large-scale refactor and overall language modification PR, originally for development of a now-removed Hindley-Milner typechecker; resolver + module graph + semcheck live here; old typechecker kept as reference in `zen/typechecker/` (dead code, eventually to be revived); classes & OOP fully removed from language |

## Build

All dev work goes through `./x.py` at the repo root (run it from there).
Requires Odin and Python (stdlib only, no pip deps).

```bash
./x.py dbg        # debug build -> bin/dbg/dzen (dzen.exe on Windows)
./x.py rel        # release build -> bin/rel/zen
./x.py chaotic    # build with -define:CHAOTIC=true -> bin/chaotic/zen
./x.py clean      # rm -rf bin/
./x.py run        # runs bin/dbg/dzen (build with dbg first); --args passes CLI flags
./x.py run --args "-dump_ast file.zn"   # example
```

- First build auto-clones+compiles `isocline` (the REPL line-editing lib)
  into `./isocline/`; needs a C compiler and network access to
  `github.com/daanx/isocline`, cached after the first build.
- `-vet -vet-tabs -strict-style -vet-style -warnings-as-errors -disallow-do`
  is baked into **both** debug and release flags in `x.py` — there is no
  separate lint step. Any code you add must pass strict style, have no
  warnings, and **never use `do`** (`do ...` is disallowed).
- Release uses `-o:aggressive -microarch:native`; a comment in `x.py` notes to
  switch to `-o:speed` if that shows weird codegen behavior.
- `CHAOTIC=true` unlocks joke features (`ifn't`, `whilen't`) that are
  intentionally untested — don't expect test coverage for them.
- Debug binary is named `dzen` (not `zen`) and has extra CLI flags defined in
  the `Options` struct in `zen/main.odin` (no release equivalent): `-dump`
  (disassemble bytecode), `-dump_ast`, `-dump_tokens`, `-trace` (trace VM
  execution), `-time` (per-stage timings), `-stress_gc`, `-log_gc`,
  `-exec "<code>"`. View them with `dzen -h`.

## Testing

```bash
./x.py test                  # unit tests + e2e suite (test/__tests__)
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
  - `test/__tests__` (default) — legacy suite whose OOP tests
    (class/inheritance/super/this) are **expected to fail** since classes are
    fully removed from the language.
  - `test/__tests_new__` (`--new`) — the active suite for ongoing work; no OOP
    or typechecking tests. **This is the working suite. Use `--new`.**
- `test/typechecking/` — orphaned tests from the typechecker era, **not wired
  into `x.py` or `run_tests.py`**. They use type-annotated syntax
  (`var x: Number`, `List[Number]`) that no longer parses, so they do not
  pass. Kept alongside `zen/typechecker/` as reference material.
- No single-file runner flag; point `run_tests.py` at a subfolder with
  `python test/run_tests.py -d <relpath>` (run from `test/`). To run one file
  by hand: build, then `bin/test/zen path/to/file.zn` and compare to the
  `// expect:`/`// ERR:` comments yourself.
- Test file format: `// expect: <line>` = expected stdout (line-for-line
  match); `// ERR: <text>` = exit must be non-zero and **only first stderr
  line** is checked (substring). `// DRAFT` anywhere in the file skips it.
- Each e2e test has a hard 2-second timeout (catches infinite loops).
- Odin unit tests live beside their subject as `zen/<name>_test.odin`
  (e.g. `chunk_test.odin`, `lexer_test.odin`), using `@(test)` procs from
  `core:testing`. No per-test filter wired up. (`type_checker_test.odin`
  lives in `zen/typechecker/` and is not compiled or run.)

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

## Current limitations (typechecker branch)

- Typechecking is **not part of the pipeline anymore** (parse → semcheck →
  module graph → resolve → codegen → VM). The HM typechecker lives on in
  `zen/typechecker/` as uncompiled reference code to be revived later; nothing
  outside that package imports it.
- Resolver output is **only partially consumed by codegen**. Codegen takes
  the `ResolutionMap` and uses it for some variable sets (`emit_variable`),
  but most variable loads/sets still re-resolve clox-style by name
  (`emit_named_variable`/`emit_named_variable_set`). The VM also re-parses
  module imports at runtime via `module_graph.odin`, even though the
  resolver already walked the graph. This is redundant but not incorrect;
  slated for a future refactor.
- Type annotations are gone from the language and the parser: `var x:
  Number` and `fn (a: Number): Number` no longer parse, and any code
  relying on them is broken until typechecking returns.
