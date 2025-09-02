# qbn

- Values: `true`, `false`, `x`, `(v, w)`
- Terms:
  - `let x = t in u`
  - `let (x, y) = v in t`
  - `sample`
  - `case v of b1 | b2 | ... end`
- Branches: `value -> term`, separated by `|`, terminated by `end`.

CLI
- Build the CLI:
  - `dune build`
- Run from build dir:
  - `_build/default/src/main.exe < file.qbn`
  - or `_build/default/src/main.exe` and type input, Ctrl-D to end.
