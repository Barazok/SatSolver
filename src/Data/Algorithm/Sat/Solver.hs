module Data.Algorithm.Sat.Solver(
    getVars
  , fromFml
--    solve
) where

import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Algorithm.Sat.Solver.CNFFml.Clause as Clause
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

fromFml :: (Eq a, Ord a) => Fml.Fml a -> CNFFml.CNFFml a
fromFml = CNFFml.mkCNFFml . aux . Fml.toCNF
    where
        aux (Fml.And a b) = aux a ++ aux b
        aux (Fml.Or a b) = [Clause.mkClause ([ c | c <- aux3 a] ++ [d | d <- aux3 b])]
            where
                aux3 (Fml.Or (Fml.Or a b) (Fml.Or c d)) = aux3 a ++ aux3 b ++ aux3 c ++ aux3 d
                aux3 (Fml.Or (Fml.Not (Fml.Final a)) (Fml.Or b c)) = [Lit.mkFalse a] ++ aux3 b ++ aux3 c
                aux3 (Fml.Or (Fml.Final a) (Fml.Or b c)) = [Lit.mkTrue a] ++ aux3 b ++ aux3 c
                aux3 (Fml.Or (Fml.Or b c) (Fml.Final a)) = aux3 b ++ aux3 c ++ [Lit.mkTrue a]
                aux3 (Fml.Or (Fml.Or b c) (Fml.Not (Fml.Final a))) = aux3 b ++ aux3 c ++ [Lit.mkFalse a]
                aux3 (Fml.Or (Fml.Not (Fml.Final a)) (Fml.Final b)) = [Lit.mkFalse a, Lit.mkTrue b]
                aux3 (Fml.Or (Fml.Final a) (Fml.Not (Fml.Final b))) = [Lit.mkTrue a, Lit.mkFalse b]
                aux3 (Fml.Or (Fml.Not (Fml.Final a)) (Fml.Not (Fml.Final b))) = [Lit.mkFalse a, Lit.mkFalse b]
                aux3 (Fml.Or (Fml.Final a) (Fml.Final b)) = [Lit.mkTrue a, Lit.mkTrue b]
                aux3 (Fml.Final  a) = [Lit.mkTrue a]
                aux3 (Fml.Not (Fml.Final  a)) = [Lit.mkFalse a]
        aux (Fml.Final  a) = [Clause.mkClause [Lit.mkTrue a]]
        aux (Fml.Not (Fml.Final  a)) = [Clause.mkClause [Lit.mkFalse a]]