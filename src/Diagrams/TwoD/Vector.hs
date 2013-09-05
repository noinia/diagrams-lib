{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Vector
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Two-dimensional vectors.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Vector
       ( -- * Special 2D vectors
         unitX, unitY, unit_X, unit_Y

         -- * Converting between vectors and angles
       , direction, fromDirection,  e

         -- * 2D vector utilities
       , perp, leftTurn
       ) where

import           Data.VectorSpace     ((<.>))
import           Data.AdditiveGroup
import           Diagrams.Coordinates
import           Diagrams.TwoD.Types

-- | The unit vector in the positive X direction.
unitX :: Num a => V2 a
unitX = 1 & 0

-- | The unit vector in the positive Y direction.
unitY :: Num a => V2 a
unitY = 0 & 1

-- | The unit vector in the negative X direction.
unit_X :: Num a => V2 a
unit_X = (-1) & 0

-- | The unit vector in the negative Y direction.
unit_Y :: Num a => V2 a
unit_Y = 0 & (-1)

-- | Compute the direction of a vector, measured counterclockwise from
--   the positive x-axis as a fraction of a full turn.  The zero
--   vector is arbitrarily assigned the direction 0.
direction                    :: (Angle a, BasicNumType a ~ b, RealFloat b) => V2 b -> a
direction (coords -> x :& y) = convertAngle . Rad $ atan2 y x

-- | Convert an angle into a unit vector pointing in that direction.
fromDirection :: (Angle a, BasicNumType a ~ b, Floating b) => a -> V2 b
fromDirection a = cos a' & sin a'
  where Rad a' = convertAngle a

-- | A convenient synonym for 'fromDirection'.
e :: (Angle a, BasicNumType a ~ b, Floating b) => a -> V2 b
e = fromDirection

-- | @perp v@ is perpendicular to and has the same magnitude as @v@.
--   In particular @perp v == rotateBy (1/4) v@.
perp :: Num a => V2 a -> V2 a
perp (coords -> x :& y) = (-y) & x

-- | @leftTurn v1 v2@ tests whether the direction of @v2@ is a left
--   turn from @v1@ (that is, if the direction of @v2@ can be obtained
--   from that of @v1@ by adding an angle 0 <= theta <= tau/2).
leftTurn :: (Ord a, Num a, AdditiveGroup a) => V2 a -> V2 a -> Bool
leftTurn v1 v2 = (v1 <.> perp v2) < 0
