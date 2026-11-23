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

`tests/` holds one test suite per challenge plus an integration suite (100 tests in total; see [Testing](#testing)), and `report.pdf` is the written report.

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) 9.6 or later (tested with 9.6.7), installable with [GHCup](https://www.haskell.org/ghcup/). Only `base` is used, so you don't need cabal, Stack or any extra packages.
- Bash to run `run-tests.sh` and `setup-check.sh` (Git Bash works on Windows)

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

## Testing

After a fresh clone, with GHC 9.6+ on your PATH, run these two commands from the repo root:

```bash
./setup-check.sh    # is everything hooked together? (toolchain + sample puzzle end to end)
./run-tests.sh      # full suite: 100 tests across 7 suites
```

**`setup-check.sh`** checks that `ghc` is on your PATH and is version 9.6 or later. It then reads the bundled puzzle in `samples/sample-puzzle.txt` (15 clues written as regex strings) and sends it through every stage: ex2 parses each clue, ex3 pretty-prints it and the text parses back to the same AST, ex5 solves the puzzle, and ex4 checks the answer against all 15 clues. Finally the answer is compared with the known solution and printed as a hexagon. It ends with `SETUP CHECK PASSED`, or it exits non-zero and names the step that failed.

**`run-tests.sh`** runs every suite, prints `PASS`/`FAIL` for each test and a pass/fail count, and exits non-zero if any test fails or a suite doesn't load.

| Suite | What it proves |
|---|---|
| `tests/TestEx1.hs` | Bounded language generation for literals, `*`, `+`, `?`, choice, sequences and `let` variables |
| `tests/TestEx2.hs` | Parsing clue strings, including wildcards and mixed operators, and returning `Nothing` for invalid input (lower-case letters, a stray `x`, an unfinished `let`) |
| `tests/TestEx3.hs` | Pretty printing with minimal parentheses and flattened structure |
| `tests/TestEx4.hs` | The solution checker, plus the grid helpers it relies on (row splitting, 120 degree rotation, three-orientation row pairing, wildcard matching) |
| `tests/TestEx5.hs` | Solving known puzzles of size 1, 3 and 5, an unsolvable puzzle, and the constraint-propagation steps (per-cell candidate sets, wildcard widening, grid rotation) |
| `tests/TestEx6.hs` | Lambda-calculus reduction counts under call-by-value and call-by-name |
| `tests/TestPipeline.hs` | Integration across modules: clue strings are parsed, solved and verified together; unsolvable puzzles and wrong answers are rejected; invalid clues are reported by number; ex4 and ex5 agree on grid layout |

To run a single suite, pass the test file together with the sources it imports:

```bash
ghc -e main tests/TestEx5.hs ex5.hs
ghc -e main tests/TestPipeline.hs tests/Pipeline.hs ex2.hs ex3.hs ex4.hs ex5.hs
```

Pass the `exN.hs` files to `ghc` explicitly, as shown above, rather than relying on `-i.`. The modules are named `Ex1`..`Ex6` but the files are lower-case, so search-path lookup only works on case-insensitive file systems. GitHub Actions (`.github/workflows/tests.yml`) runs both scripts on every push to `main` and on every pull request.

Known parser limits, which the tests leave alone on purpose: ex2 raises an error rather than returning `Nothing` for `.*`, a leading `*` or an unmatched `)`, and it accepts some malformed input such as `A(`. That's why the sample puzzle writes its clues without `.*`.
