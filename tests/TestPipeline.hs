-- Integration tests: clue strings go through every stage together
-- (ex2 parse -> ex3 unparse -> ex5 solve -> ex4 check).
-- Run from the repo root (run-tests.sh does this for you):
--   ghc -e main tests/TestPipeline.hs tests/Pipeline.hs ex2.hs ex3.hs ex4.hs ex5.hs

import Data.Either (isLeft)

import Pipeline
import qualified Ex4
import qualified Ex5

size3Clues :: [String]
size3Clues =
    [ "(H|T|U|V)+", "D(AW|OG)", "(Z|F|M|S)+"
    , ".S", "H(A|E)Z", "(T|W|M|L)+"
    , "(W|R|Z|G)+", "(SA|T|PN|X)+", "(O|N|D|H)+"
    ]

main :: IO ()
main = do
    putStrLn "=== Integration: parse -> unparse -> solve -> check ===\n"

    (sampleClues, sampleExpected) <- readPuzzleFile "samples/sample-puzzle.txt"

    check "Sample puzzle file has 15 clues and an expected answer"
        (15, Just "POUELUCKWOOLESOEKLY") (length sampleClues, sampleExpected)

    check "Every sample clue parses"
        True (either (const False) (const True) (parseClues sampleClues))

    check "Sample clues survive an ex3 unparse / ex2 parse round trip"
        True (either (const False) (all roundTrips) (parseClues sampleClues))

    check "Size 5 sample: solved and verified from clue strings"
        (Right (PipelineResult "POUELUCKWOOLESOEKLY" (replicate 15 True)))
        (runPipeline sampleClues)

    check "Size 3 with wildcard clue: solved and verified from clue strings"
        (Right (PipelineResult "HTDAWSZ" (replicate 9 True)))
        (runPipeline size3Clues)

    check "Size 1: solved and verified from clue strings"
        (Right (PipelineResult "Q" [True, True, True]))
        (runPipeline ["Q+", "(Q|R)", "Q?Q"])

    check "Unsolvable size 3 (every clue a different letter): checker rejects solver output"
        False
        (either (const True) (and . prChecks) (runPipeline (map (\c -> [c, '+']) "ABCDEFGHI")))

    check "Unsolvable size 1 (A+ vs B+): solver returns no answer"
        (Right "") (fmap prSolution (runPipeline ["A+", "B+", "A+"]))

    check "Invalid clue (lower-case letter) is reported, not solved"
        True (isLeft (runPipeline ["A+", "b+", "A+"]))

    check "Invalid clue error names the failing clue"
        (Left "clue 2 does not parse: \"b+\"") (runPipeline ["A+", "b+", "A+"])

    check "Checker flags a wrong answer to the sample puzzle"
        False
        (either (const True) (and . checkAnswer "QOUELUCKWOOLESOEKLY") (parseClues sampleClues))

    check "ex4 and ex5 agree on the hexagon row structure for n = 1, 3, 5, 7, 9"
        (map Ex4.cellRowStructure [1, 3, 5, 7, 9]) (map Ex5.cellRowStructure [1, 3, 5, 7, 9])

    check "ex4 and ex5 rotate the grid the same way"
        (Ex4.rotateHexgrid ["POU", "ELUC", "KWOOL", "ESOE", "KLY"] [3, 4, 5, 4, 3])
        (Ex5.rotateHexgrid ["POU", "ELUC", "KWOOL", "ESOE", "KLY"] [3, 4, 5, 4, 3])

    check "hexRows splits the sample answer into 3-4-5-4-3 rows"
        ["POU", "ELUC", "KWOOL", "ESOE", "KLY"] (hexRows 15 "POUELUCKWOOLESOEKLY")

    putStrLn "\n=== All integration tests complete ===\n"

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check description expected result = do
    let status = if result == expected then "PASS" else "FAIL"
    putStrLn $ "Test: " ++ description
    putStrLn $ "Expected: " ++ show expected
    putStrLn $ "Output:   " ++ show result
    putStrLn $ "Status:   " ++ status
    putStrLn ""
