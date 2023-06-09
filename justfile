oc := "odin"
target_dbg := "bin/dbg/zen"
target_rel := "bin/rel/zen"
modules := "src/ src/chunk src/lexer src/vm src/value src/compiler src/debug"

alias r := rel

_default: dbg

_predbg:
    @ mkdir -p bin/dbg/ || true

_prerel:
    @ mkdir -p bin/rel/ || true

_predoc:
    @ mkdir -p doc/ || true

_pretest:
    @ mkdir -p bin/test/ || true

# Create a debug build.
dbg *args="": _predbg
    {{ oc }} build src/ -out:{{ target_dbg }} -debug {{ args }}

# Create a release build optimized for speed.
rel *args="": _prerel
    {{ oc }} build src/ -out:{{ target_rel }} -o:speed {{ args }}

# Generate documentation in the doc/ folder.
doc: _predoc
    odin doc {{ modules }} > doc/docs.txt

# Test the provided module.
test module="": _pretest
    #!/usr/bin/env bash
    cd bin/test/
    {{ oc }} test ../../src/{{ module }}

# Test all modules.
test_all: _pretest
    #!/usr/bin/env bash
    cd bin/test/
    {{ oc }} test ../../src/chunk
    {{ oc }} test ../../src/lexer

# Run the program with optional args.
run *args="": dbg
    ./{{ target_dbg }} {{ args }}
