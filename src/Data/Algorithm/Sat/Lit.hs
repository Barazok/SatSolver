module Data.Algorithm.Sat.Lit
  (
  -- * Type
  Lit(..)

  -- * Making
, mkFalse
, mkTrue
, mkAllValues

  -- * Querying
, getVar

  -- * Transforming
, neg
  ) where

import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.Algorithm.Sat.Fml as Fml

-- |Literal type definition.
data Lit a = F (Var.Var a) | T (Var.Var a) deriving (Eq, Ord)

instance (Show a) => Show (Lit a) where
  show (F v) = "-" ++ show v
  show (T v) =        show v

-- |'mkFalse' @v@ returns the false literal on variable @v@.
--
-- >>> let v = Var.mk 'a' in mkFalse v
-- -Var 'a'
mkFalse :: Var.Var a -> Lit a
mkFalse = F

-- |'mkTrue' @v@ returns the true literal on variable @v@.
--
-- >>> let v = Var.mk 'a' in mkTrue v
-- Var 'a'
mkTrue :: Var.Var a -> Lit a
mkTrue = T

-- |'neg' @l@ negates litteral @v@.
--
-- >>> let v = Var.mk 'a' in neg (mkFalse v)
-- Var 'a'
-- >>> let v = Var.mk 'a' in neg (mkTrue v)
-- -Var 'a'
neg :: Lit a -> Lit a
neg (F v) = T v
neg (T v) = F v

-- |'getVar' @l@ returns the variable on which literal @l@ is defined.
--
-- >>> let v = Var.mk 'a' in getVar (mkFalse v)
-- Var 'a'
-- >>> let v = Var.mk 'a' in getVar (mkTrue v)
-- Var 'a'
getVar :: Lit a -> Var.Var a
getVar (F v) = v
getVar (T v) = v

mkAllValues :: (Eq a) => [(Var.Var a, Bool)] -> [Lit a]
mkAllValues [] = []
mkAllValues [(v,b)]
  | b == True = [mkTrue v]
  | b == False = [mkFalse v]
mkAllValues ((v,b):ls)
  | b == True = [mkTrue v] ++ mkAllValues ls
  | b == False = [mkFalse v] ++ mkAllValues ls