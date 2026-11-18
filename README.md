# Haskell Regex Puzzle Solver

Haskell solutions to six challenges built around regular expressions and a hexagonal **Regexle** puzzle, plus a lambda-calculus evaluator.

> Haskell coursework for the University of Southampton (2025).

| File | Challenge |
|---|---|
| `ex1.hs` | **Bounded language generation.** Enumerate every string up to length *k* that a regex (with `let`-bound sub-expressions) accepts |
| `ex2.hs` | **Regex parser.** Tokenise and parse a regex string into an `LRegExp` AST |
| `ex3.hs` | **Pretty printer.** Turn an `LRegExp` back into a string, using minimal parentheses and flattening nested structure |
| `ex4.hs` | **Solution checker.** Check a filled hexagonal grid against every row's regex constraint |
| `ex5.hs` | **Regexle solver.** Solve a hexagonal Regexle puzzle from its constraints |
| `ex6.hs` | **Lambda calculus with exceptions.** Count reduction steps under call-by-value and call-by-name, up to a bound |

`tests/` holds one test suite per challenge (60 tests in total), and `report.pdf` is the written report.

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) 9.6 or later (tested with 9.6.7), installable with [GHCup](https://www.haskell.org/ghcup/). Only `base` is used, so you don't need cabal, Stack or any extra packages.
- Bash to run `run-tests.sh` (Git Bash works on Windows)

No API keys or environment variables are needed.

## Build

There's nothing to build ahead of time. Each challenge is a single module (`ex1.hs` defines `module Ex1`, and so on), and you can type-check or compile one on its own:

```bash
ghc -c ex5.hs
```

## Run

Load a challenge in GHCi and call its functions:

```bash
ghci ex1.hs
ghci> boundedLang 3 (Let [] (Star (Lit 'A')))
["","A","AA","AAA"]
```

## Test

```bash
./run-tests.sh                              # run every challenge's tests (60 in total)
ghc -e main tests/TestEx5.hs ex5.hs         # run one challenge's tests
```

Each test prints `PASS` or `FAIL`. `run-tests.sh` exits non-zero if any test fails or a suite doesn't load. Pass the `exN.hs` file to `ghc` explicitly, as shown above, rather than relying on `-i.`: the modules are named `Ex1`..`Ex6` but the files are lower-case, so search-path lookup only works on case-insensitive file systems.
