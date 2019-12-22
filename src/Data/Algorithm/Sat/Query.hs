module Data.Algorithm.Sat.Query (
    -- satisfiable,
    -- satisfyingAssignment,
    -- satisfyingAssignments,
    -- tautology
    ) where

import qualified Data.Maybe as Maybe
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Solver as Solver
import qualified Data.Algorithm.Sat.Solver.CNFFml as CNFFml
import qualified Data.Algorithm.Sat.Solver.CNFFml.Clause as Clause

satisfiable :: (Ord a) => Fml.Fml a -> Bool
satisfiable = Maybe.isJust . satisfyingAssignment

satisfyingAssignment :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
satisfyingAssignment = Maybe.listToMaybe . satisfyingAssignments

satisfyingAssignments :: (Ord a) => Fml.Fml a -> [Assignment.Assignment a]
satisfyingAssignments f = []

-- tautology :: (Ord a) => Fml.Fml a -> Bool
-- tautology