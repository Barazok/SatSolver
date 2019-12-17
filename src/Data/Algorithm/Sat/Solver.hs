module Data.Algorithm.Sat.Solver(
    getVars,
--    solve
) where

import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Map.Strict as Map


getVars :: Fml.Fml a -> [Fml.Fml a]
getVars f
    | Fml.Or a b    <- f = getVars a ++ getVars b
    | Fml.And a b   <- f = getVars a ++ getVars b
    | Fml.Imply a b <- f = getVars a ++ getVars b
    | Fml.Equiv a b <- f = getVars a ++ getVars b
    | Fml.XOr a b   <- f = getVars a ++ getVars b
    | Fml.Not (Fml.Final a)   <- f = [f]
    | Fml.Not a     <- f = getVars a
    | Fml.Final a   <- f = [f]

-- |'solve' @f@ calculate an assignment of the propositional variables that makes
--              the formula f logically true
-- solve :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a) 
solve f = maximum (getVars (Fml.toCNF f))

{- solve (Fml.Final a) = Just (Assignment.insert (Lit.mkTrue a) empty)
solve (Fml.Not (Fml.Final a)) = Just (Assignment.insert (Lit.mkFalse a) empty)
solve (Fml.Or a b) = solve a  -}