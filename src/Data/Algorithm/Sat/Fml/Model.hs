module Data.Algorithm.Sat.Fml.Model (
    atLeast
) where

import qualified Data.Algorithm.Sat.Fml as Fml
import qualified Data.Algorithm.Sat.Var as Var

atLeast :: Int -> [Fml.Fml a] -> Fml.Fml a
atLeast k fs
    | k >= length fs = Fml.multAnd fs
    | k == 1 = Fml.multOr fs
    | otherwise = fs !! 1

-- anyOf :: (Ord a)=>[Fml.Fml a]-> Fml.Fml a

-- noneOf :: (Ord a)=>[Fml.Fml a]-> Fml.Fml a

-- allOf :: (Ord a)=>[Fml.Fml a]-> Fml.Fml a

-- exactlyOneOf :: (Ord a)=>[Fml.Fml a]-> Fml.Fml a