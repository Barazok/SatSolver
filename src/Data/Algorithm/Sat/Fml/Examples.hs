module Data.Algorithm.Sat.Fml.Examples
  (
  fml1
, fml2
, fml3
, fml4
, fml5
, fml6
, fml7
, fml8
, fml9
, fml10
-- , fml11
-- , fml12
-- , fml13
  ) where

import qualified Data.Algorithm.Sat.Fml       as Fml
import qualified Data.Algorithm.Sat.Fml.Model as Fml.Model

vA = Fml.mkVar 'A'
vB = Fml.mkVar 'B'
vC = Fml.mkVar 'C'
vD = Fml.mkVar 'D'
vE = Fml.mkVar 'E'
vF = Fml.mkVar 'F'

-- C + -(BC)
fml1 :: Fml.Fml Char
fml1 = Fml.Or vC (Fml.Not (Fml.And vB vC))

-- (-A)(-B)(-A + B)(-B + B)
fml2 :: Fml.Fml Char
fml2 = Fml.multAnd [f, g, h]
  where
    f = Fml.And (Fml.Not vA) (Fml.Not vB)
    g = Fml.Or (Fml.Not vA) vB
    h = Fml.Or (Fml.Not vB) vB

-- (A + C)(AD + A(-D)) + AC + C
fml3 :: Fml.Fml Char
fml3 = Fml.multOr [f, g, h]
  where
    f = Fml.And (Fml.Or vA vC) (Fml.Or (Fml.And vA vD) (Fml.And vA (Fml.Not vD)))
    g = Fml.And vA vC
    h = vC

-- (-A)(A + B) + (B + AA)(A + (-B)):
fml4 :: Fml.Fml Char
fml4 = Fml.Or f g
  where
    f = Fml.And (Fml.Not vA) (Fml.Or vA vB)
    g = Fml.And (Fml.Or vB (Fml.And vA vA)) (Fml.Or vA (Fml.Not vB))

-- (A => (B + C)((-C) + D)) + (BC => (-A)(-D))
fml5 :: Fml.Fml Char
fml5 = Fml.Or f g
  where
    f = Fml.Imply vA (Fml.And (Fml.Or vB vC) (Fml.Or (Fml.Not vC) vD))
    g = Fml.Imply (Fml.And vB vC) (Fml.And (Fml.Not vA) (Fml.Not vD))

-- -(-(A)) + -(-(-B))
fml6 :: Fml.Fml Char
fml6 = Fml.Or f g
  where
    f = Fml.Not (Fml.Not vA)
    g = Fml.Not (Fml.Not (Fml.Not vB))

-- (AB + CD + EF) <=> (AB => (CD <=> EF))
fml7 :: Fml.Fml Char
fml7 = Fml.Equiv f g
  where
    f = Fml.multOr [Fml.And vA vB, Fml.And vC vD, Fml.And vE vF]
    g = Fml.Imply (Fml.And vA vB) (Fml.Equiv (Fml.And vC vD) (Fml.And vE vF))

-- (A+B) (A+(-B)) ((-A)+B) ((-A)+(-B))
fml8 :: Fml.Fml Char
fml8 = Fml.multAnd [f, g, h, i]
  where
    f = Fml.Or vA vB
    g = Fml.Or vA (Fml.Not vB)
    h = Fml.Or (Fml.Not vA) vB
    i = Fml.Or (Fml.Not vA) (Fml.Not vB)

-- (A(-B) + EF) (AB => ((-B)C + B(-C) + (-F)) (AB <=> C(-D)) (AB => (CD + E(-F)))
fml9 :: Fml.Fml Char
fml9 = Fml.multAnd [f, g, h, i]
  where
    f = Fml.Or (Fml.And vA (Fml.Not vB)) (Fml.And vE vF)
    g = Fml.Imply (Fml.And vA vB) (Fml.multOr [Fml.And (Fml.Not vB) vC, Fml.And vB (Fml.Not vC), Fml.Not vF])
    h = Fml.Equiv (Fml.And vA vB) (Fml.And vC (Fml.Not vD))
    i = Fml.Imply (Fml.And vA vB) (Fml.Or (Fml.And vC vD) (Fml.And vE (Fml.Not vF)))

fml10 :: Fml.Fml Char
fml10 = Fml.multAnd [p1, p2, p3, p4]
  where
    p1 = Fml.Model.atLeast 3 [vA, vB, vC, vD, vE, vF]
    p2 = Fml.multOr [Fml.Not vA, Fml.Not vB, vC, vF]
    p3 = Fml.multOr [Fml.Not vB, Fml.Not vD, Fml.Not vE]
    p4 = Fml.Imply (Fml.Or vA vB) (Fml.And vC vE)

-- fml11 :: Fml.Fml Char
-- fml11 = Fml.And p1 p2
--   where
--     p1 = Fml.multOr [vA, vB, vC, vD, vE, vF]
--     p2 = Fml.Model.noneOf [vA, vC, vE]

-- fml12 :: Fml.Fml Char
-- fml12 = Fml.And p1 p2
--   where
--     p1 = Fml.And (Fml.Or (Fml.Not vA) (Fml.Not vB)) (Fml.Or vC (Fml.Not vB))
--     p2 = Fml.Model.allOf [vA, vB]

-- fml13 :: Fml.Fml Char
-- fml13 = Fml.multAnd [p1, p2, p3, p4]
--   where
--     p1 = Fml.Model.exactlyOneOf [vA, vB, vC]
--     p2 = Fml.And vA (Fml.Not vE)
--     p3 = Fml.Imply (Fml.Model.allOf [vA, vB]) (Fml.Model.allOf [vC, vE])
--     p4 = Fml.Equiv (Fml.And vA vC) (Fml.multAnd [vB, vE, vF])
