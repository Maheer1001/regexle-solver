-- End-to-end setup check. Run it with ./setup-check.sh from the repo root.
--
-- Proves the toolchain works and every stage is wired together: it reads the
-- bundled sample puzzle (samples/sample-puzzle.txt), parses each clue string
-- with ex2, round-trips it through the ex3 pretty printer, solves the puzzle
-- with ex5, verifies the answer with the ex4 checker, and compares it with
-- the known solution. Exits non-zero if any step fails.

import Control.Monad (unless, when)
import Data.Version (showVersion)
import System.Exit (exitFailure)
import System.Info (compilerName, compilerVersion, os, arch)

import Pipeline

samplePath :: FilePath
samplePath = "samples/sample-puzzle.txt"

main :: IO ()
main = do
    putStrLn "=== Setup check: regexle-solver end to end ==="
    putStrLn $ "Toolchain: " ++ compilerName ++ " " ++ showVersion compilerVersion
            ++ " (" ++ os ++ "/" ++ arch ++ ")"

    (clues, expected) <- readPuzzleFile samplePath
    putStrLn $ "Puzzle:    " ++ samplePath ++ " (" ++ show (length clues) ++ " clues)"
    when (null clues || length clues `mod` 3 /= 0) $
        failWith "puzzle must have a non-zero multiple of 3 clues"

    parsed <- either failWith return (parseClues clues)
    putStrLn "[ok] ex2 parsed every clue"

    let badRoundTrips = [c | (c, e) <- zip clues parsed, not (roundTrips e)]
    unless (null badRoundTrips) $
        failWith ("ex3 unparse/ex2 parse round trip changed: " ++ show badRoundTrips)
    putStrLn "[ok] ex3 unparse -> ex2 parse round trip is stable"

    let solution = solveClues parsed
        checks = checkAnswer solution parsed
    putStrLn $ "[ok] ex5 solver returned " ++ show solution

    unless (and checks && length checks == length clues) $
        failWith ("ex4 checker rejected the solution: " ++ show checks)
    putStrLn $ "[ok] ex4 checker accepts the solution against all "
            ++ show (length checks) ++ " clues"

    case expected of
        Just e | e /= solution -> failWith ("expected " ++ show e ++ " but got " ++ show solution)
               | otherwise     -> putStrLn "[ok] solution matches the known answer"
        Nothing                -> putStrLn "[--] no expected answer in the puzzle file"

    putStrLn "\nVerified solution:"
    let rows = hexRows (length clues) solution
        width = maximum (map length rows)
    mapM_ (\r -> putStrLn ("  " ++ replicate (width - length r) ' ' ++ unwords (map (:[]) r))) rows
    putStrLn "\nSETUP CHECK PASSED"

failWith :: String -> IO a
failWith msg = do
    putStrLn $ "[FAIL] " ++ msg
    putStrLn "SETUP CHECK FAILED"
    exitFailure
