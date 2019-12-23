module Data.Algorithm.Sat.Query (
    satisfiable,
    satisfyingAssignment,
    satisfyingAssignments,
    truthtable,
    getAllTrue,
    getAFalse,
    tautology
    ) where

import qualified Data.Maybe as Maybe
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Fml as Fml
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
truthtable e = [(bs, evaluate e bs) | bs <- booltable (Fml.vars e)]

getAllTrue :: (Eq a) => Fml.Fml a -> [([(Var.Var a, Bool)], Bool)]
getAllTrue = filter (\(_,a) -> a == True) . truthtable

getAFalse :: (Eq a) => Fml.Fml a -> Maybe ([(Var.Var a, Bool)], Bool)
getAFalse = Maybe.listToMaybe . filter (\(_,a) -> a == False) . truthtable

satisfyingAssignments :: (Ord a) => Fml.Fml a -> [Assignment.Assignment a]
satisfyingAssignments f = [Assignment.insertAll (Lit.mkAllValues n) Assignment.mkEmpty | n <- [fst n | n <- getAllTrue f]]

tautology :: (Ord a) => Fml.Fml a -> Bool
tautology = not . Maybe.isJust . getAFalse