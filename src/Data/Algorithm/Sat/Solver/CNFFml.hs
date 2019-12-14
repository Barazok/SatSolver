module Data.Algorithm.Sat.Solver.CNFFml (
    CNFFml (..)
    ) where

import qualified Data.Algorithm.Sat.Solver.CNFFml.Clause as Solver.Clause

newtype CNFFml a = CNFFml { getClauses :: [Solver.Clause.Clause a] }