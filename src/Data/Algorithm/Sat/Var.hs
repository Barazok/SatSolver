module Data.Algorithm.Sat.Var
  (
  -- * Type
  Var (..)

  -- * Making
, mk
  ) where

-- |@Var@ type definition.
newtype Var a = Var { getName :: a } deriving (Eq, Ord)

instance (Show a) => Show (Var a) where
  show Var { getName = n } = "Var " ++ show n

-- |'mk' @x@ makes the variable with name @x@.
--
-- >>> mk 'a'
-- Var 'a'
mk :: a -> Var a
mk = Var
