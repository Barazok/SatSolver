module Data.Algorithm.Sat.Solver(
    solve
) where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Map as Map

-- |'solve' @f@ calculate an assignment of the propositional variables that makes
--              the formula f logically true
solve :: (Ord a) => Fml.Fml a -> Maybe (Assignment.Assignment a)
solve f = 

setLitteral :: (Ord a) => Fml.Fml a -> Map Fml.fml [Integer]
setLitteral f
    | Final a <- f = Assignment.insert a Assignment.mkEmpty

getMaxFromMap :: (Ord a) => Map Fml.Fml [Integer] => Maybe (Fml.Fml a)
getMaxFromMap m = go [] Nothing (Map.toList m)
  where
    go ks _        []           = ks 
    go ks Nothing  ((k,v):rest) = go (k:ks) (Just v) rest
    go ks (Just u) ((k,v):rest)
        | v < u     = go ks     (Just u) rest
        | v > u     = go [k]    (Just v) rest
        | otherwise = go (k:ks) (Just v) rest