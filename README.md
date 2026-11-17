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

## Run

Requires GHC, for example via [GHCup](https://www.haskell.org/ghcup/).

```bash
./run-tests.sh                   # run every challenge's tests
runghc -i. tests/TestEx5.hs      # run one challenge's tests
ghci ex5.hs                      # load a challenge interactively
```
