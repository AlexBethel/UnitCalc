# UnitCalc

UnitCalc is a terminal-based calculator program with support for
dimensional analysis and advanced mathematics. While its primary
intended use is as a terminal-based REPL, the program can also be
automated as a relatively basic scripting language interpreter.

## Invocation

The basic invocation is as follows:
```
unitcalc [<options>] [<script_file> [<script_file> [...]]]
```
If any `script_file`s are specified, UnitCalc will execute each of
them in the order they are specified, then exit; otherwise, if no
`script_file`s are specified (or the `--interactive` option is
passed), the program gives an interactive interpreter.

### Options

* `--help`, `-h`: Print a help string, then exit immediately.
* `--interactive`, `-i`: Force the program to give an interactive
  shell rather than just silently exiting after executing supplied
  script files. This does nothing if no script files are given.
* `--no-prelude`, `-n`: Do not load any standard unit definitions.
* `--quiet`, `-q`: Do not print the default banner on startup.
* `--version`, `-v`: Print UnitCalc's version, then exit immediately.

## Language
