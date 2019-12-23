module Data.Algorithm.Sat.Fml.Model (
    atLeast
  , anyOf
  , noneOf
  , allOf
  , exactlyOneOf
) where

import qualified Data.List as List
import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.Algorithm.Sat.Query as Query

atLeast :: (Ord a) => Int -> [Fml.Fml a] -> Fml.Fml a
atLeast i f = Fml.multAnd $ take i (map (fst) (List.filter ((== True).snd) [(c, Query.satisfiable c) | c <- f]))

anyOf :: (Ord a)=>[Fml.Fml a]-> Fml.Fml a
anyOf f = (take 1 (map (fst) (List.filter ((== True).snd) [(c, Query.satisfiable c) | c <- f]))) !! 0

noneOf :: (Ord a)=>[Fml.Fml a]-> Fml.Fml a
noneOf f = Fml.multAnd [Fml.Not c | c <- f]

allOf :: (Ord a)=>[Fml.Fml a]-> Fml.Fml a
allOf f = Fml.multAnd [c | c <- f]

exactlyOneOf :: (Ord a) => [Fml.Fml a]-> Fml.Fml a
exactlyOneOf f = (take 1 (map (fst) (List.filter ((== True).snd) [(c, Query.satisfiable c) | c <- f]))) !! 0