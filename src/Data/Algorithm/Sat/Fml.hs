module Data.Algorithm.Sat.Fml (
    -- * Type
    Fml (..)

    -- * Creating
    , mkVar
    -- * Converting
    , toCNF
    , multAnd
    , multOr
    -- * Querying
    , vars
    -- * Showing
    , prettyPrinter
) where

import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.List as List

-- |Formula type definition.
data Fml a = Or     (Fml a) (Fml a)
           | And    (Fml a) (Fml a)
           | Imply  (Fml a) (Fml a)
           | Equiv  (Fml a) (Fml a)
           | XOr    (Fml a) (Fml a)
           | Not    (Fml a)
           | Final  (Var.Var a)
           deriving (Show,Eq,Ord)

-- |'toCNF' @f@ Transform f to Conjonctive Normal Formula (CNF).
toCNF :: Fml a -> Fml a
toCNF =  aux . reduce
    where
        aux = Not

reduce :: Fml a -> Fml a
reduce f
    | Equiv a b         <- f = And (Or (reduce a) (Not (reduce b))) (Or (Not (reduce a)) (reduce b))
    | Imply a b         <- f = Or  (Not (reduce a)) (reduce b)
    | XOr   a b         <- f = And (Or (reduce a) (reduce b)) (Or (Not (reduce a)) (Not (reduce b)))
    | Or    a b         <- f = Or (reduce a) (reduce b)
    | And   a b         <- f = And (reduce a) (reduce b)
    | Not   a           <- f = Not (reduce a)
    | Final a           <- f = f

getVars :: Fml a -> [Var.Var a]
getVars f
    | Or a b    <- f = getVars a ++ getVars b
    | And a b   <- f = getVars a ++ getVars b
    | Imply a b <- f = getVars a ++ getVars b
    | Equiv a b <- f = getVars a ++ getVars b
    | XOr a b   <- f = getVars a ++ getVars b
    | Not a     <- f = getVars a
    | Final a   <- f = [a]

-- |'vars' @f@ returns all variable which are in @f@.
--
-- >>> let v = vars (And (Final (mk 'A')) (Final (mk 'B')))
-- [Var 'A', Var 'B']
vars :: (Eq a) => Fml a -> [Var.Var a]
vars f = List.nub (getVars f)

-- |'prettyPrinter' @f@ returns a pretty string of @f@
prettyPrinter :: (Show a) => Fml a -> String
prettyPrinter f
    | Or    a b <- f = "("    ++ prettyPrinter a ++ " OR "    ++ prettyPrinter b ++ ")"
    | And   a b <- f = "("    ++ prettyPrinter a ++ " AND "   ++ prettyPrinter b ++ ")"
    | Imply a b <- f = "("    ++ prettyPrinter a ++ " IMPLY " ++ prettyPrinter b ++ ")"
    | Equiv a b <- f = "("    ++ prettyPrinter a ++ " EQUIV " ++ prettyPrinter b ++ ")"
    | XOr   a b <- f = "("    ++ prettyPrinter a ++ " XOR "   ++ prettyPrinter b ++ ")"
    | Not   a   <- f = "NOT " ++ prettyPrinter a
    | Final a   <- f = show a

-- |'mkVar' @v@ returns a formula with @v@ value
--
-- >>> let v = mkVar 'A'
-- Var 'A'
mkVar :: a -> Fml a
mkVar a = Final (Var.mk a)

-- |'multOr' @v@ returns a Or formula composed by all formulas
multOr :: [Fml a] -> Fml a
multOr [f] = f
multOr (f:fs) = Or f (multOr fs)

-- |'multAnd' @v@ returns a And formula composed by all formulas
multAnd :: [Fml a] -> Fml a
multAnd [f] = f
multAnd (f:fs) = And f (multAnd fs)