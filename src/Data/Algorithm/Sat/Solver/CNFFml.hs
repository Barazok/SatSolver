module Data.Algorithm.Sat.Solver.CNFFml (
    CNFFml (..)
  , mkCNFFml
  , uniqueCNFFml
  , numberClauseInCNFFml
  , getAllClauses
  , haveOneLitInCNFFml
    ) where
import qualified Data.Algorithm.Sat.Solver.CNFFml.Clause as Solver
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.List as L

newtype CNFFml a = CNFFml { getClauses :: [Solver.Clause a] }

instance (Show a) => Show (CNFFml a) where
    show CNFFml { getClauses = ls } = "(" ++ L.intercalate "," (L.map show ls) ++ ")"

mkCNFFml :: [Solver.Clause a] -> CNFFml a
mkCNFFml ls = CNFFml ls

numberClauseInCNFFml :: CNFFml a -> Int
numberClauseInCNFFml (CNFFml ls) = L.length ls

uniqueCNFFml :: CNFFml a -> Bool
uniqueCNFFml c = 1 == numberClauseInCNFFml c

getAllClauses :: CNFFml a -> [Solver.Clause a]
getAllClauses (CNFFml c) = c

haveOneLitInCNFFml :: CNFFml a -> Bool
haveOneLitInCNFFml c = 0 < L.length (filter Solver.uniqueClause (getAllClauses c))
