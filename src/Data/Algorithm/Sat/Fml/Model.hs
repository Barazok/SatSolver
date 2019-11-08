module Data.Algorithm.Sat.Fml.Model (
    atLeast
) where

import qualified Data.Algorithm.Sat.Fml as Fml

atLeast :: (Eq t, Num t, Ord a) => t -> [Fml.Fml a] -> Fml.Fml a
atLeast k fs
    | k >= length fs = Fml.multAnd fs
    | k == 1 = Fml.multOr fs
    | otherwise = 

-- anyOf :: (Ord a)=>[Fml.Fml a]-> Fml.Fml a

-- noneOf :: (Ord a)=>[Fml.Fml a]-> Fml.Fml a

-- allOf :: (Ord a)=>[Fml.Fml a]-> Fml.Fml a

-- exactlyOneOf :: (Ord a)=>[Fml.Fml a]-> Fml.Fml a