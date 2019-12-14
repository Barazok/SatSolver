import qualified Data.List as L
import qualified Data.Tuple as T
import qualified Data.Algorithm.Sat.Lit as Lit
import qualified Data.Algorithm.Sat.Var as Var
import qualified Data.Map.Strict as M

newtype Assignment a = Assignment { getMap :: M.Map (Var.Var a) Bool }

instance (Show a, Ord a) => Show (Assignment a) where
    show = show . L.sort . L.map (Arrow.first Var.getName) . M.toList . getMap

-- |’empty’ return the empty assignment.
mkEmpty :: Assignment a
mkEmpty = Assignment { getMap = M.empty }

-- |’lookup’ @v@ @m@ returns the boolean value asociated to variable @v@ in
-- the asignement @m@ (if it exists).
lookup :: (Ord a) => Var.Var a -> Assignment a -> Maybe Bool
lookup v = M.lookup v . getMap

-- |’insert’ @l@ @m@ inserts literal @l@ in the assignment @m@ producing a new
-- assigment.
insert :: (Ord a) => Lit.Lit a -> Assignment a -> Assignment a
insert (Lit.F v) = Assignment . M.insert v False . getMap
insert (Lit.T v) = Assignment . M.insert v True . getMap
