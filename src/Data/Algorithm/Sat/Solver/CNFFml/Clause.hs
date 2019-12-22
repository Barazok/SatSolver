module Data.Algorithm.Sat.Solver.CNFFml.Clause (
    Clause (..),
    mkClause
  , uniqueClause
  , numberLitInClause
  , getLiterals
  , removeLiteralInClause
    ) where
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.List as L

-- A clause is a list of literals.
newtype Clause a = Clause { getLits :: [Lit.Lit a] } deriving (Eq)
instance (Show a) => Show (Clause a) where
    show Clause { getLits = ls } = "(" ++ L.intercalate " | " (L.map show ls) ++ ")"

mkClause :: (Ord a) => [Lit.Lit a] -> Clause a
mkClause ls = Clause ls

numberLitInClause :: Clause a -> Int
numberLitInClause (Clause ls) = L.length ls

uniqueClause :: Clause a -> Bool
uniqueClause c = 1 == numberLitInClause c

getLiterals :: Clause a -> [Lit.Lit a]
getLiterals (Clause c) = c

removeLiteralInClause :: (Ord a) => Lit.Lit a -> Clause a -> Clause a
removeLiteralInClause lit cl
  | lit `L.elem` (getLiterals cl) = mkClause []
  | otherwise = cl