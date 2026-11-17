{-
Author: Maheer Mirza
Student ID: mmm3g24
Copyright: University of Southampton 2025
Description: Lambda calculus evaluation with exceptions (Call-by-Value vs Call-by-Name)
-}

module Ex6 where

-- Expression data type for lambda calculus with exceptions
data Exp = Var Int | Abs Int Exp | App Exp Exp | Exn | Catch Exp Exp
    deriving (Eq,Show,Read)

-- | Count reduction steps for both evaluation strategies
-- Returns (CBV steps, CBN steps) bounded by the given limit
countReductions :: Int -> Exp -> (Int, Int)
countReductions bound expr = (cbvCount, cbnCount)
    where
        cbvCount = countCBV bound 0 expr
        cbnCount = countCBN bound 0 expr

-- Count Call-by-Value reduction steps
countCBV :: Int -> Int -> Exp -> Int
countCBV bound acc expr
    | acc >= bound = bound
    | isValue expr = acc
    | otherwise = case reduceCBV expr of
        Nothing -> acc
        Just expr' -> countCBV bound (acc + 1) expr'

-- Count Call-by-Name reduction steps
countCBN :: Int -> Int -> Exp -> Int
countCBN bound acc expr
    | acc >= bound = bound
    | isValue expr = acc
    | otherwise = case reduceCBN expr of
        Nothing -> acc
        Just expr' -> countCBN bound (acc + 1) expr'

-- Check if expression is a value (lambda abstraction or exception)
isValue :: Exp -> Bool
isValue (Abs _ _) = True
isValue Exn = True
isValue _ = False

-- Call-by-Value reduction: evaluate arguments before substitution
reduceCBV :: Exp -> Maybe Exp
reduceCBV (Var _) = Nothing
reduceCBV (Abs _ _) = Nothing  
reduceCBV Exn = Nothing
reduceCBV (App Exn _) = Just Exn
reduceCBV (App _ Exn) = Just Exn
reduceCBV (App (Abs n body) e2) 
    | Just e2' <- reduceCBV e2 = Just (App (Abs n body) e2')
    | otherwise = Just (substitute n e2 body)
reduceCBV (App e1 e2)
    | Just e1' <- reduceCBV e1 = Just (App e1' e2)
    | Just e2' <- reduceCBV e2 = Just (App e1 e2')
    | otherwise = Nothing
reduceCBV (Catch e1 e2)
    | Just e1' <- reduceCBV e1 = Just (Catch e1' e2)
    | Just e2' <- reduceCBV e2 = Just (Catch e1 e2')
    | e2 == Exn = Just e1
    | isValue e2 = Just e2
    | otherwise = Nothing

-- Call-by-Name reduction: substitute before evaluating arguments
reduceCBN :: Exp -> Maybe Exp
reduceCBN (Var _) = Nothing
reduceCBN (Abs _ _) = Nothing  
reduceCBN Exn = Nothing
reduceCBN (App Exn _) = Just Exn
reduceCBN (App _ Exn) = Just Exn
reduceCBN (App (Abs n body) e2) 
    | e2 /= Exn = Just (substitute n e2 body)
    | otherwise = Nothing
reduceCBN (App e1 e2)
    | Just e1' <- reduceCBN e1 = Just (App e1' e2)
    | Just e2' <- reduceCBN e2 = Just (App e1 e2')
    | otherwise = Nothing
reduceCBN (Catch e1 e2)
    | Just e1' <- reduceCBN e1 = Just (Catch e1' e2)
    | Just e2' <- reduceCBN e2 = Just (Catch e1 e2')
    | e2 == Exn = Just e1
    | isValue e2 = Just e2
    | otherwise = Nothing

-- Substitute variable with replacement expression
substitute :: Int -> Exp -> Exp -> Exp
substitute var replacement = sub
    where
        sub (Var v) 
            | v == var = replacement
            | otherwise = Var v
        sub (Abs v body) 
            | v == var = Abs v body
            | otherwise = Abs v (sub body)
        sub (App e1 e2) = App (sub e1) (sub e2)
        sub Exn = Exn
        sub (Catch e1 e2) = Catch (sub e1) (sub e2)
