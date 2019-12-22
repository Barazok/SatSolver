module Data.Algorithm.Sat.Query (
    satisfiable,
    satisfyingAssignment,
    satisfyingAssignments,
    truthtable,
    getAllTrue,
    extractAllFirst,
    -- tautology
    ) where

import qualified Data.Maybe as Maybe
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Solver as Solver
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Algorithm.Sat.Solver.CNFFml.Clause as Clause
import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.List as List

satisfiable :: (Ord a) => Fml.Fml a -> Bool
satisfiable = Maybe.isJust . satisfyingAssignment

satisfyingAssignment :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
satisfyingAssignment = Maybe.listToMaybe . satisfyingAssignments

-- evaluates an expression
evaluate :: (Eq a) => Fml.Fml a -> [(Var.Var a, Bool)] -> Bool
evaluate (Fml.Final f) bs = Maybe.fromJust (lookup f bs)
evaluate (Fml.Not e) bs = not (evaluate e bs)
evaluate (Fml.And e1 e2) bs = evaluate e1 bs && evaluate e2 bs
evaluate (Fml.Or e1 e2) bs = evaluate e1 bs || evaluate e2 bs

bools = [True, False]

-- all possible combinations of variable assignments
booltable :: [Var.Var v] -> [[(Var.Var v, Bool)]]
booltable [] = [[]]
booltable (a:as) = [(a,b) : r | b <- bools, r <- booltable as]

-- variable assignments and corresponding evaluation of an expression
truthtable :: (Eq a) => Fml.Fml a -> [([(Var.Var a, Bool)], Bool)]
truthtable e = [(bs, evaluate e bs) | bs <- booltable (Solver.getVars e)]

getAllTrue :: (Eq a) => Fml.Fml a -> [([(Var.Var a, Bool)], Bool)]
getAllTrue = filter (\(_,a) -> a == True) . truthtable . Fml.toCNF

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) | x `elem` xs = rmdups xs
              | otherwise = x : rmdups xs

extractAllFirst :: (Eq a) => Fml.Fml a -> [(Var.Var a, Bool)]
extractAllFirst f = rmdups (concat [fst n | n <- getAllTrue f])

satisfyingAssignments :: (Ord a) => Fml.Fml a -> [Assignment.Assignment a]
satisfyingAssignments f = [aux n | n <- extractAllFirst f]
    where
        aux (v,True) = Assignment.insert (Lit.mkTrue v) Assignment.mkEmpty
        aux (v,False) = Assignment.insert (Lit.mkFalse v) Assignment.mkEmpty

-- tautology :: (Ord a) => Fml.Fml a -> Bool
-- tautology