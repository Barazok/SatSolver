module Data.Algorithm.Sat.Solver.CNFFml.Clause (
    Clause (..)
    ) where
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.List as L

-- A clause is a list of literals.
newtype Clause a = Clause { getLits :: [Lit.Lit a] } deriving (Eq)
instance (Show a) => Show (Clause a) where
    show Clause { getLits = ls } = "(" ++ L.intercalate "," (L.map show ls) ++ ")"