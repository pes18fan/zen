# zen

Programming language.

# development

## building

Make sure the Odin compiler and Python are installed. Then, run the
build script with the `dbg` arg to create a debug build:

```bash
./x.py dbg
```

This will produce a debug build as `./bin/dbg/zen`. To produce a release
build, run:

```bash
./x.py rel
```

## testing

This project includes unit tests written using Odin's own testing library
as well as a custom end-to-end tester written in Ruby. To test everything
at once, run:

```bash
./x.py test
```
