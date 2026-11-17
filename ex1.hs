{-
Author: Maheer Mirza
Student ID: mmm3g24
Copyright: University of Southampton 2025
Description: Regular expression bounded language generator
-}

module Ex1 where

import Data.Maybe (fromMaybe)

-- Data type definitions
data LRegExp = Let [(Int,RegExp)] RegExp
    deriving (Eq,Show,Read)

data RegExp = S SRegExp | Star SRegExp | Plus SRegExp | Seq [RegExp]
    deriving (Eq,Show,Read)

data SRegExp = Empty | Lit Char | Opt Char | Any | OptAny | SSeq [SRegExp] | Choice [SRegExp] | Var Int
    deriving (Eq,Show,Read)

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
