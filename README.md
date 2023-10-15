# zen

A lightweight dynamically typed programming language written in Odin.

# development

## building

### requirements

- [Odin](https://odin-lang.org)
- Python

### steps

Run the build script with the `dbg` arg to create a debug build:

```bash
./x.py dbg
```

This will produce a debug build as `./bin/dbg/zen`. To produce a release
build, run:

```bash
./x.py rel
```

## testing

**Requires:** Ruby

This project includes unit tests written using Odin's own testing library
as well as a custom end-to-end tester written in Ruby. To test everything
at once, run:

```bash
./x.py test
```
