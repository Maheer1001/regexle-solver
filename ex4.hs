{-
Author: Maheer Mirza
Student ID: mmm3g24
Copyright: University of Southampton 2025
Description: Hexagonal Regexle validator
-}

module Ex4 where

import Data.Maybe (fromMaybe)

-- Data type definitions
data LRegExp = Let [(Int,RegExp)] RegExp
    deriving (Eq,Show,Read)

data RegExp = S SRegExp | Star SRegExp | Plus SRegExp | Seq [RegExp]
    deriving (Eq,Show,Read)

data SRegExp = Empty | Lit Char | Opt Char | Any | OptAny | SSeq [SRegExp] | Choice [SRegExp] | Var Int
    deriving (Eq,Show,Read)

-- | Check if a hexagonal grid solution matches all regular expression constraints
-- Returns a list of booleans indicating whether each constraint is satisfied
checkRegexle :: String -> [LRegExp] -> [Bool]
checkRegexle answer constraints =
    let pairs = constraintPairing answer constraints
    in map (uncurry matchesLRegExp) pairs

-- Check if a string matches an LRegExp (considering wildcards)
matchesLRegExp :: String -> LRegExp -> Bool
matchesLRegExp str lregexp = any (wildcardMatch str) (boundedLang (length str) lregexp)

-- Match a string against a pattern containing wildcards (.)
wildcardMatch :: String -> String -> Bool
wildcardMatch [] [] = True
wildcardMatch (c:cs) ('.':ps) = wildcardMatch cs ps
wildcardMatch (c:cs) (p:ps) = c == p && wildcardMatch cs ps
wildcardMatch _ _ = False

-- Number of orientations in hexagonal grid (horizontal, diagonal left, diagonal right)
orientations :: Int
orientations = 3

-- Calculate side length from number of constraints
sideLengths :: [LRegExp] -> Int
sideLengths constraints = length constraints `div` orientations

-- Generate the row structure for a hexagonal grid of given side length
-- For size n, rows have pattern: [n, n+1, ..., 2n-1, ..., n+1, n]
cellRowStructure :: Int -> [Int]
cellRowStructure 1 = [1]
cellRowStructure n =
    let left = [(n+1) `div` 2..n]
        right = tail (reverse left)
    in left ++ right

-- Format a flat string into hexagonal grid structure based on row lengths
hexgridFormat :: [a] -> [Int] -> [[a]]
hexgridFormat = go []
    where
        go acc [] [] = []
        go acc [] [0] = [reverse acc]
        go acc [] (y:ys) = []
        go acc (x:xs) [] = []
        go acc (x:xs) (0:ys) = reverse acc : go [] (x:xs) ys
        go acc (x:xs) (y:ys) = go (x:acc) xs (y-1:ys)

-- Rotate hexagonal grid by 120 degrees to extract rows in different orientation
rotateHexgrid :: [String] -> [Int] -> [String]
rotateHexgrid _ [] = []
rotateHexgrid input (r:rs) =
    let (updated, newRow) = consumeHeads input r
    in newRow : rotateHexgrid updated rs

-- Consume first characters from specific rows to form new row after rotation
consumeHeads :: [String] -> Int -> ([String],String)
consumeHeads input n =
    let
        nonEmptyIndices = [i | (i, row) <- zip [0..] input, not (null row)]
        rowsToConsume = take n (reverse nonEmptyIndices)
        heads = map (\i -> head (input !! i)) rowsToConsume
        updated = [if i `elem` rowsToConsume then tail row else row
                    | (i, row) <- zip [0..] input]
    in (updated, heads)

-- Pair each row (in all three orientations) with its corresponding constraint
constraintPairing :: String -> [LRegExp] -> [(String,LRegExp)]
constraintPairing answer constraints =
    let sides = sideLengths constraints
        rowStructure = cellRowStructure sides
        grid = hexgridFormat answer rowStructure
        grid2 = rotateHexgrid grid rowStructure
        grid3 = rotateHexgrid grid2 rowStructure
        answerSet = grid ++ map reverse grid2 ++ map reverse grid3
    in zip answerSet constraints

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
                -- Stops when all generated strings exceed the bound
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
