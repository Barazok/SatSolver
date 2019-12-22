module Data.Algorithm.Sat.Solver.CNFFml (
    CNFFml (..)
  , mkCNFFml
  , uniqueCNFFml
  , numberClauseInCNFFml
  , getAllClauses
  , haveOneLitInCNFFml
  , mostFrequentLit
  , reduceClauses
  , getOneLitInCNFFml
  , solveCNFFml
    ) where
import qualified Data.Algorithm.Sat.Solver.CNFFml.Clause as Solver
import qualified Data.Algorithm.Sat.Assignment as Assignment
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.List as L
import qualified Data.Ord as O

newtype CNFFml a = CNFFml { getClauses :: [Solver.Clause a] }

instance (Show a) => Show (CNFFml a) where
    show CNFFml { getClauses = ls } = L.intercalate " & " (L.map show ls)

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

mostFrequentLit :: (Eq a, Ord a) =>  CNFFml a -> Lit.Lit a
mostFrequentLit d = head (L.maximumBy (O.comparing L.length) (L.group (L.sort (L.concat [Solver.getLiterals c | c <- getAllClauses d]))))

reduceClauses :: (Ord a) => Lit.Lit a -> CNFFml a -> CNFFml a
reduceClauses l c =  mkCNFFml [d | d <- [Solver.removeLiteralInClause (l) e | e <- getAllClauses c], Solver.numberLitInClause d > 0]

getOneLitInCNFFml :: (Ord a, Eq a) => CNFFml a -> Lit.Lit a
getOneLitInCNFFml c = (Solver.getLiterals ((filter (Solver.uniqueClause ) (getAllClauses c) !! 0) )) !! 0

solveCNFFml :: (Ord a, Eq a) => CNFFml a -> Bool
solveCNFFml f = if haveOneLitInCNFFml f
  then aux (reduceClauses (getOneLitInCNFFml f) f)
  else aux (reduceClauses (mostFrequentLit f) f)
  where 
    aux f = numberClauseInCNFFml f == 0