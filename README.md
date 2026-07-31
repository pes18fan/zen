# zen

A lightweight programming language written in Odin.

<p align="center">
    <img src="https://github.com/pes18fan/zen/raw/main/example.png" alt="zen code example" />
</p>

Check out more code in `/examples`!

# features

- first-class functions and closures
- garbage collection
- file-based module system for code organization
- static typing with Hindley-Milner type inference (experimental)
- simple standard library

# documentation

All documentation about the language is in the Markdown file `DOCUMENTATION.md`
in the root of this repository.

A man page written in Markdown is available in `etc/zen.1.md`. To convert it to
a format usable by `man`, use `pandoc` to convert as such:

```bash
pandoc -s -t man ./etc/zen.1.md -o zen.1
```

# development

The script `x.py` in the root of this repository is where you will go for
most of your development work on zen. It is used for building, testing,
running benchmarks and generating documentation.

## building

### requirements

- [Odin](https://odin-lang.org)
- Python

zen uses [isocline](https://github.com/daanx/isocline) for its REPL. Downloading
and setting up the library for use is handled by the `x.py` build script
automatically.

### steps

Run the build script with the `dbg` arg to create a debug build:

```bash
./x.py dbg
```

This will produce a debug build as `./bin/dbg/dzen`. To produce a release
build, run:

```bash
./x.py rel
```

## testing

The requirements are the same as for building.

### steps

To test everything at once, run:

```bash
./x.py test
```

# contributing

If you find a bug or want to suggest something, feel free to open an issue or
pull request!
