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

### requirements

- Odin
- Ruby

### steps

To test everything at once, run:

```bash
./x.py test
```

# contributing

zen may have a lot of bugs and problems lying around, feel free to open an issue or create a pull request if you find any!
