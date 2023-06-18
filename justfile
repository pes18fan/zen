oc := "odin"
target_dbg := "bin/dbg/zen"
target_rel := "bin/rel/zen"

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
    {{ oc }} build src/ -out:{{ target_dbg }} -debug -o:none {{ args }}

# Create a release build optimized for speed.
rel *args="": _prerel
    {{ oc }} build src/ -out:{{ target_rel }} -o:speed {{ args }}

# Generate documentation in the doc/ folder.
doc: _predoc
    odin doc src/ > doc/docs.txt

# Test all modules, with both Odin's built-in tester and the ruby tester.
test_all: _pretest
    #!/usr/bin/env bash
    cd bin/test/
    odin test ../../src/
    cd ../../testing/
    echo ""
    if ! command -v ruby >/dev/null 2>&1; then
        echo "Ruby needed to run the tests!"
        exit 1
    fi
    cd testing
    ruby ./run_tests.rb

# Run the program with optional args.
run *args="": dbg
    ./{{ target_dbg }} {{ args }}
