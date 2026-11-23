-- Glue used by the integration tests and the setup check: takes puzzle clues
-- as regex strings and runs them through every stage of the project.
--
--   ex2 (parse) -> ex3 (unparse round trip) -> ex5 (solve) -> ex4 (check)
--
-- Each challenge module declares its own copy of the LRegExp types, so values
-- are moved between modules with `read . show` (all of them derive Show/Read
-- with identical constructors). This is test tooling only; it does not change
-- any solver behaviour.

module Pipeline
    ( parseClues
    , parseClue
    , roundTrips
    , solveClues
    , checkAnswer
    , runPipeline
    , PipelineResult (..)
    , readPuzzleFile
    , hexRows
    ) where

import qualified Ex2
import qualified Ex3
import qualified Ex4
import qualified Ex5

data PipelineResult = PipelineResult
    { prSolution :: String
    , prChecks   :: [Bool]
    } deriving (Eq, Show)

-- | Parse one clue with the ex2 parser.
parseClue :: String -> Maybe Ex2.LRegExp
parseClue = Ex2.parseRegExp

-- | Parse every clue, reporting the first (1-based) clue that fails.
parseClues :: [String] -> Either String [Ex2.LRegExp]
parseClues clues = mapM parseOne (zip [1 :: Int ..] clues)
    where
        parseOne (i, c) = maybe (Left ("clue " ++ show i ++ " does not parse: " ++ show c))
                                Right (parseClue c)

-- | ex3 pretty-prints the ex2 AST, and parsing that text again gives back the
-- same AST.
roundTrips :: Ex2.LRegExp -> Bool
roundTrips e = parseClue (Ex3.unparseRegExp (convert e)) == Just e

-- | Solve already-parsed clues with the ex5 solver.
solveClues :: [Ex2.LRegExp] -> String
solveClues es = Ex5.solveRegexle (map convert es)

-- | Check an answer against parsed clues with the ex4 checker.
checkAnswer :: String -> [Ex2.LRegExp] -> [Bool]
checkAnswer answer es = Ex4.checkRegexle answer (map convert es)

-- | Parse, solve and check in one go.
runPipeline :: [String] -> Either String PipelineResult
runPipeline clues = do
    es <- parseClues clues
    let solution = solveClues es
    return (PipelineResult solution (checkAnswer solution es))

-- | Read a puzzle file: one clue per line, '#' comment lines, blank lines
-- ignored. A comment of the form "# expected: XYZ" gives the known answer.
readPuzzleFile :: FilePath -> IO ([String], Maybe String)
readPuzzleFile path = do
    contents <- readFile path
    let ls = map trim (lines contents)
        clues = [l | l <- ls, not (null l), head l /= '#']
        expected = case [drop (length tag) l | l <- ls, take (length tag) l == tag] of
                       (e:_) -> Just (trim e)
                       []    -> Nothing
    length contents `seq` return (clues, expected)
    where
        tag = "# expected:"
        trim = reverse . dropWhile (`elem` " \t\r") . reverse . dropWhile (`elem` " \t\r")

-- | Split a flat answer into hexagon rows for display.
hexRows :: Int -> String -> [String]
hexRows numClues answer = go (Ex4.cellRowStructure (numClues `div` 3)) answer
    where
        go [] _ = []
        go (n:ns) s = let (row, rest) = splitAt n s in row : go ns rest

convert :: (Show a, Read b) => a -> b
convert = read . show
