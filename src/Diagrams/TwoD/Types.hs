{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Types
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Basic types for two-dimensional Euclidean space.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Types
       ( -- * 2D Euclidean space
         V2(..), v2, unv2
       , R2, r2, unr2

       , P2, p2, unp2
       , P2D, p2d, unp2d

       , T2 , T2D

         -- * Angles
       , Angle(..)
       , Turn(..), CircleFrac, Rad(..), Deg(..)
       , fullCircle, convertAngle
       , TurnD

       , HasBasicNumType(..), IsBasicNumType
       ) where

import           Diagrams.Coordinates
import           Diagrams.Core
import           Diagrams.Util           (tau)

import           Data.AffineSpace.Point
import           Data.Basis
import           Data.NumInstances.Tuple ()
import           Data.VectorSpace

import           Data.Typeable

------------------------------------------------------------
-- 2D Euclidean space

-- | The two-dimensional Euclidean vector space R^2.  This type is
--   intentionally abstract.
--
--   * To construct a vector, use 'v2', or '&' (from "Diagrams.Coordinates"):
--
-- @
-- r2 (3,4) :: V2 a
-- 3 & 4    :: V2 a
-- @
--
--     Note that "Diagrams.Coordinates" is not re-exported by
--     "Diagrams.Prelude" and must be explicitly imported.
--
--   * To construct the vector from the origin to a point @p@, use
--     @p 'Data.AffineSpace..-.' 'origin'@.
--
--   * To convert a vector @v@ into the point obtained by following
--     @v@ from the origin, use @'origin' 'Data.AffineSpace..+^' v@.
--
--   * To convert a vector back into a pair of components, use 'unv2'
--     or 'coords' (from "Diagrams.Coordinates").  These are typically
--     used in conjunction with the @ViewPatterns@ extension:
--
-- @
-- foo (unr2 -> (x,y)) = ...
-- foo (coords -> x :& y) = ...
-- @

data V2 a = V2 a a
  deriving (Eq, Ord, Typeable)

instance Num a => AdditiveGroup (V2 a) where
  zeroV = V2 0 0
  V2 x1 y1 ^+^ V2 x2 y2 = V2 (x1 + x2) (y1 + y2)
  negateV (V2 x y) = V2 (-x) (-y)

instance Num a => Num (V2 a) where
  (+)                 = (^+^)
  V2 x1 y1 * V2 x2 y2 = V2 (x1 * x2) (y1 * y2)  -- this is sort of bogus
  (-)                 = (^-^)
  negate              = negateV
  abs (V2 x y)        = V2 (abs x) (abs y)
  signum (V2 x y)     = V2 (signum x) (signum y)
  fromInteger i       = V2 i' i'
    where i' = fromInteger i

instance Fractional a => Fractional (V2 a) where
  V2 x1 y1 / V2 x2 y2 = V2 (x1/x2) (y1/y2)
  recip (V2 x y) = V2 (recip x) (recip y)
  fromRational r = V2 r' r'
    where r' = fromRational r

instance (Eq a, Num a, Show a) => Show (V2 a) where
  showsPrec p (V2 x y) = showParen (p >= 7) $
    showCoord x . showString " & " . showCoord y
   where
    isNegative c = not $ c == abs c
    showCoord c | isNegative c = showParen True (shows c)
                | otherwise    = shows x

instance Read a => Read (V2 a) where
  readsPrec d r = readParen (d > app_prec)
                  (\rr -> [ (V2 x y, r''')
                          | (x,r')    <- readsPrec (amp_prec + 1) rr
                          , ("&",r'') <- lex r'
                          , (y,r''')  <- readsPrec (amp_prec + 1) r''
                          ])
                  r
    where
      app_prec = 10
      amp_prec = 7


v2       :: (a,a) -> V2 a
v2 (x,y) = V2 x y

unv2          :: V2 a -> (a, a)
unv2 (V2 x y) = (x,y)


type R2 = V2 Double

-- | Construct a 2D vector from a pair of components.  See also '&'.
r2 :: (Double, Double) -> R2
r2 = v2

-- | Convert a 2D vector back into a pair of components.  See also 'coords'.
unr2 :: R2 -> (Double, Double)
unr2 = unv2

type instance V (V2 a) = (V2 a)

instance Num a => VectorSpace (V2 a) where
  type Scalar (V2 a) = a
  s *^ V2 x y = V2 (s*x) (s*y)

instance Num a => HasBasis (V2 a) where
  type Basis (V2 a) = Either () () -- = Basis (Double, Double)
  basisValue (Left () )          = V2 1 0
  basisValue (Right ())          = V2 0 1

  decompose (V2 x y)             = [(Left (), x), (Right (), y)]

  decompose' (V2 x _) (Left ())  = x
  decompose' (V2 _ y) (Right ()) = y

instance (Num a, AdditiveGroup a) => InnerSpace (V2 a) where
  (V2 x1 y1) <.> (V2 x2 y2) = x1*x2 + y1*y2

instance Coordinates (V2 a) where
  type FinalCoord (V2 a)     = a
  type PrevDim (V2 a)        = a
  type Decomposition (V2 a)  = a :& a

  x & y           = V2 x y
  coords (V2 x y) = x :& y

-- | Points in R^2.  This type is intentionally abstract.
--
--   * To construct a point, use 'p2', or '&' (see
--     "Diagrams.Coordinates"):
--
-- @
-- p2 (3,4)  :: P2
-- 3 & 4     :: P2
-- @
--
--   * To construct a point from a vector @v@, use @'origin' 'Data.AffineSpace..+^' v@.
--
--   * To convert a point @p@ into the vector from the origin to @p@,
--   use @p 'Data.AffineSpace..-.' 'origin'@.
--
--   * To convert a point back into a pair of coordinates, use 'unp2',
--     or 'coords' (from "Diagrams.Coordinates").  It's common to use
--     these in conjunction with the @ViewPatterns@ extension:
--
-- @
-- foo (unp2 -> (x,y)) = ...
-- foo (coords -> x :& y) = ...
-- @

type P2 a = Point (V2 a)

type P2D = Point R2


-- | Construct a 2D point from a pair of coordinates.  See also '&'.
p2 :: (a,a) -> P2 a
p2 = P . v2

-- | Convert a 2D point back into a pair of coordinates.  See also 'coords'.
unp2      :: P2 a -> (a,a)
unp2 (P v) = unv2 v


-- | Construct a 2D point from a pair of coordinates.  See also '&'.
p2d :: (Double, Double) -> P2D
p2d = p2

-- | Convert a 2D point back into a pair of coordinates.  See also 'coords'.
unp2d :: P2D -> (Double, Double)
unp2d = unp2

-- | Transformations in R^2.
type T2 a = Transformation (V2 a)

instance Num a => Transformable (V2 a) where
  transform = apply

type T2D = T2 Double

------------------------------------------------------------
-- Angles

-- | Newtype wrapper used to represent angles as fractions of a
--   circle.  For example, 1\/3 turn = tau\/3 radians = 120 degrees.
newtype Turn a = Turn { getTurn :: a }
  deriving (Read, Show, Eq, Ord, Enum, Fractional, Num, Real, RealFrac)


instance IsBasicNumType a => HasBasicNumType (Turn a) where
    type BasicNumType (Turn a) = a

-- | Type Synonym for Turn
type TurnD = Turn Double

-- | Deprecated synonym for 'TurnD', retained for backwards compatibility.
type CircleFrac = Turn Double






-- | Newtype wrapper for representing angles in radians.
newtype Rad a = Rad { getRad :: a}
  deriving (Read, Show, Eq, Ord, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

instance IsBasicNumType a => HasBasicNumType (Rad a) where
    type BasicNumType (Rad a) = a


-- | Newtype wrapper for representing angles in degrees.
newtype Deg a = Deg { getDeg :: a }
  deriving (Read, Show, Eq, Ord, Enum, Fractional, Num, Real, RealFrac)

instance IsBasicNumType a => HasBasicNumType (Deg a) where
    type BasicNumType (Deg a) = a




-- | Type class for types that measure angles.
class (Num a, HasBasicNumType a) => Angle a where

  -- | Convert to a turn, /i.e./ a fraction of a circle.
  toTurn   :: a -> Turn (BasicNumType a)

  -- | Convert from a turn, /i.e./ a fraction of a circle.
  fromTurn :: Turn (BasicNumType a) -> a


instance IsBasicNumType a => Angle (Turn a) where
  toTurn   = id
  fromTurn = id

-- | tau radians = 1 full turn.
instance (IsBasicNumType a, Floating a) => Angle (Rad a) where
  toTurn   = Turn . (/tau) . getRad
  fromTurn = Rad . (*tau) . getTurn

-- | 360 degrees = 1 full turn.
instance (IsBasicNumType a, Fractional a) => Angle (Deg a) where
  toTurn   = Turn . (/360) . getDeg
  fromTurn = Deg . (*360) . getTurn

-- | An angle representing one full turn.
fullTurn :: Angle a => a
fullTurn = fromTurn 1

-- | Deprecated synonym for 'fullTurn', retained for backwards compatibility.
fullCircle :: Angle a => a
fullCircle = fullTurn

-- | Convert between two angle representations.
convertAngle :: (Angle a, Angle b, BasicNumType b ~ BasicNumType a) => a -> b
convertAngle = fromTurn . toTurn


-----------------------------------------------------------------------------

-- | A class expressing that a type is usable as a basic num type
class Num c => IsBasicNumType c


-- The `standard` numeric types are basicNum types
instance IsBasicNumType Double
instance IsBasicNumType Integer
instance IsBasicNumType Int

-- | A class expressing that this type is somehow parameterized over
-- an underlying numeric type
class IsBasicNumType (BasicNumType t) => HasBasicNumType t where
    type BasicNumType t
