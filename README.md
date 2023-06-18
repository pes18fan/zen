# zen

Programming language.

# development

## building

Make sure `odin` and `just` are installed and working. Clone the repo,
then run:

```bash
just
```

This will produce a debug build as `./bin/dbg/zen`. To produce a release
build, run:

```bash
just rel
```

## testing

This project includes unit tests written using Odin's own testing library
as well as a custom end-to-end tester written in Ruby. To test everything
at once, run:

```bash
just test_all
```
