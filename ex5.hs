{-
Author: Maheer Mirza
Student ID: mmm3g24
Copyright: University of Southampton 2025
Description: Hexagonal Regexle solver
-}

module Ex5 where

import Data.Maybe (fromMaybe)
import Data.List (nub, intersect)

-- Data type definitions
data LRegExp = Let [(Int,RegExp)] RegExp
    deriving (Eq,Show,Read)

data RegExp = S SRegExp | Star SRegExp | Plus SRegExp | Seq [RegExp]
    deriving (Eq,Show,Read)

data SRegExp = Empty | Lit Char | Opt Char | Any | OptAny | SSeq [SRegExp] | Choice [SRegExp] | Var Int
    deriving (Eq,Show,Read)

-- | Solve a hexagonal Regexle puzzle given constraints
-- Returns a string representing the solution (top to bottom, left to right)
solveRegexle :: [LRegExp] -> String
solveRegexle expressions =
    let n = size expressions
        structure = cellRowStructure n
        possibilities = analyseCellPositions (generatePossibilities expressions structure)
        (gridOne, rest) = splitAt n possibilities
        (gridTwo, gridThree) = splitAt n rest
        rotationOne = rotateHexgrid gridOne structure
        intersectOne = zipWith (zipWith intersect) rotationOne gridTwo
        rotationTwo = rotateHexgrid intersectOne structure
        intersectTwo = zipWith (zipWith intersect) rotationTwo gridThree
        solution = rotateNTimes intersectTwo structure 4
    in concat [take 1 pos | row <- solution, pos <- row]

-- Number of orientations in hexagonal grid
orientations :: Int
orientations = 3

-- Calculate hexagon side length from number of constraints
size :: [LRegExp] -> Int
size expressions = length expressions `div` orientations

-- Generate the row structure for a hexagonal grid of given side length
cellRowStructure :: Int -> [Int]
cellRowStructure 1 = [1]
cellRowStructure n =
    let left = [(n+1) `div` 2..n]
        right = tail (reverse left)
    in left ++ right

-- Rotate hexagonal grid n times
rotateNTimes :: [[[Char]]] -> [Int] -> Int -> [[[Char]]]
rotateNTimes grid _ 0 = grid
rotateNTimes grid struct n = rotateNTimes (rotateHexgrid grid struct) struct (n-1)

-- | Generate all symbolic strings up to length k accepted by the regex
boundedLang :: Int -> LRegExp -> [String]
boundedLang bound (Let bindings mainExpr) = expandRegExp expandedBindings mainExpr
    where
        -- Resolve all variable bindings eagerly to avoid circular dependencies
        expandedBindings :: [(Int, [String])]
        expandedBindings = [(n, expandRegExp expandedBindings expr) | (n, expr) <- bindings]
        
        -- Expand a top-level RegExp to its string language
        expandRegExp :: [(Int, [String])] -> RegExp -> [String]
        expandRegExp env expr = case expr of
            S sregexp -> expandSRegExp env sregexp
            Star sregexp -> expandStar env sregexp
            Plus sregexp -> expandPlus env sregexp
            Seq exprs -> expandSeq env exprs
        
        -- Expand a simple regex to its string language
        expandSRegExp :: [(Int, [String])] -> SRegExp -> [String]
        expandSRegExp env sregexp = case sregexp of
            Empty -> [""]
            Lit c -> [[c]]
            Any -> ["."]
            OptAny -> ["", "."]
            Opt c -> ["", [c]]
            SSeq sexprs -> expandSSeq env sexprs
            Choice sexprs -> expandChoice env sexprs
            Var n -> fromMaybe [] (lookup n env)
        
        -- Expand Plus (one or more repetitions) using iterative generation
        expandPlus :: [(Int, [String])] -> SRegExp -> [String]
        expandPlus env sregexp = base ++ generateReps base
            where
                base = expandSRegExp env sregexp
                -- Recursively generate longer strings by concatenating with base
                generateReps :: [String] -> [String]
                generateReps prev =
                    let next = [s1 ++ s2 | s1 <- prev, s2 <- base, length (s1 ++ s2) <= bound]
                    in if null next then [] else next ++ generateReps next
        
        -- Star is zero or more: empty string plus all Plus results
        expandStar :: [(Int, [String])] -> SRegExp -> [String]
        expandStar env sregexp = "" : expandPlus env sregexp
        
        -- Expand sequence of simple regexes using Cartesian product
        expandSSeq :: [(Int, [String])] -> [SRegExp] -> [String]
        expandSSeq _ [] = [""]
        expandSSeq env sexprs =
            let allStrings = [expandSRegExp env se | se <- sexprs]
            in [concat strs | strs <- sequence allStrings, length (concat strs) <= bound]
        
        -- Expand sequence of top-level regexes
        expandSeq :: [(Int, [String])] -> [RegExp] -> [String]
        expandSeq env exprs =
            let allStrings = [expandRegExp env e | e <- exprs]
            in [concat strs | strs <- sequence allStrings, length (concat strs) <= bound]
        
        -- Choice is union of all alternatives
        expandChoice :: [(Int, [String])] -> [SRegExp] -> [String]
        expandChoice env = concatMap (expandSRegExp env)

-- Generate all possible strings for each constraint
generatePossibilities :: [LRegExp] -> [Int] -> [[String]]
generatePossibilities expressions structure =
    [filter (\s -> length s == len) (boundedLang len constraint)
        | (len, constraint) <- zip (concat $ replicate 3 structure) expressions]

-- Diagnostic function to analyse constraint possibilities
analyseConstraints :: [LRegExp] -> IO ()
analyseConstraints expressions = do
    let structure = cellRowStructure (size expressions)
    let results = generatePossibilities expressions structure
    mapM_ (\(i, constraint, possibilities) -> do
        putStrLn $ "Constraint " ++ show i ++ ": " ++ show constraint
        putStrLn $ "Possibilities: " ++ show possibilities
        putStrLn ""
        ) (zip3 [1..] expressions results)

-- Analyse which characters are possible at each cell position
analyseCellPositions :: [[String]] -> [[[Char]]]
analyseCellPositions rowPossibilities =
    [getCharsAtPosition i possibilities
        | (i, possibilities) <- zip [1..] rowPossibilities]
    where
        getCharsAtPosition :: Int -> [String] -> [[Char]]
        getCharsAtPosition constraintNum strs =
            let maxLen = maximum (map length strs)
                positions = [nub [s !! i | s <- strs, i < length s] | i <- [0..maxLen-1]]
                expandedPositions = [expandWildcards pos | pos <- positions]
                n = length rowPossibilities `div` 3
            in if constraintNum > n
            then reverse expandedPositions
            else expandedPositions

-- Expand wildcard '.' to all possible letters
expandWildcards :: String -> String
expandWildcards str =
    if '.' `elem` str
    then ['A'..'Z']
    else str

-- Rotate hexagonal grid by 120 degrees
rotateHexgrid :: [[a]] -> [Int] -> [[a]]
rotateHexgrid _ [] = []
rotateHexgrid input (r:rs) =
    let (updated, newRow) = consumeHeads input r
    in newRow : rotateHexgrid updated rs

-- Consume first elements from specific rows during rotation
consumeHeads :: [[a]] -> Int -> ([[a]], [a])
consumeHeads input n =
    let nonEmptyIndices = [i | (i, row) <- zip [0..] input, not (null row)]
        rowsToConsume = take n (reverse nonEmptyIndices)
        heads = map (\i -> head (input !! i)) rowsToConsume
        updated = [if i `elem` rowsToConsume then tail row else row
                    | (i, row) <- zip [0..] input]
    in (updated, heads)
