{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Offset
-- Copyright   :  (c) 2012 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Compute offsets to segments in two dimensions.
-- 
-----------------------------------------------------------------------------
module Diagrams.TwoD.Offset 
    ( offsetSegment
    ) where

import Data.AffineSpace
import Data.Monoid.Inf
import Data.VectorSpace

import Diagrams.Core

import Diagrams.Located
import Diagrams.Parametric
import Diagrams.Path
import Diagrams.Segment
import Diagrams.Trail
import Diagrams.TwoD.Curvature
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Types

perp :: R2 -> R2
perp v = rotateBy (-1/4) v

unitPerp :: R2 -> R2
unitPerp = normalized . perp

perpAtParam :: Segment Closed R2 -> Double -> R2
perpAtParam   (Linear (OffsetClosed a))  t = unitPerp a 
perpAtParam s@(Cubic _ _ _)              t = unitPerp a
  where
    (Cubic a _ _) = snd $ splitAtParam s t

-- | Compute the offset of a segment.  Given a segment compute the offset
--   curve that is a fixed distance from the original curve.  For linear
--   segments nothing special happens, the same linear segment is returned
--   with a point that is offset by a perpendicular vector of the given offset
--   length.
--
--   Cubic segments require a search for a subdivision of cubic segments that
--   gives an approximation of the offset within the given epsilon tolerance.
--   We must do this because the offset of a cubic is not a cubic itself (the
--   degree of the curve increases).  Cubics do, however, approach constant
--   curvature as we subdivide.  In light of this we scale the handles of
--   the offset cubic segment in proportion to the radius of curvature difference
--   between the original subsegment and the offset which will have a radius
--   increased by the offset parameter.
--
--   In the following example the blue lines are the original segments and
--   the alternating green and red lines are the resulting offset trail segments.
--
--   <<diagrams/src_Diagrams_TwoD_Offset_cubicOffsetExample.svg#diagram=cubicOffsetExample&width=600>>
--
--   Note that when the original curve has a cusp, the offset curve forms a
--   radius around the cusp, and when there is a loop in the original curve,
--   there can be two cusps in the offset curve.
--
offsetSegment :: Double     -- ^ Epsilon value that represents the maximum 
                            --   allowed deviation from the true offset.  In
                            --   the current implementation each result segment
                            --   should be bounded by arcs that are plus or
                            --   minus epsilon from the radius of curvature of
                            --   the offset.
              -> Double     -- ^ Offset from the original segment, positive is
                            --   on the right of the curve, negative is on the
                            --   left.
              -> Segment Closed R2  -- ^ Original segment
              -> Located (Trail R2) -- ^ Resulting located (at the offset) trail.
offsetSegment _       r s@(Linear (OffsetClosed a))    = trailFromSegments [s] `at` origin .+^ va
  where va = r *^ unitPerp a

offsetSegment epsilon r s@(Cubic a b (OffsetClosed c)) = t `at` origin .+^ va
  where
    t = trailFromSegments (go (radiusOfCurvature s 0.5))
    -- Perpendiculars to handles.
    va = r *^ unitPerp a
    vc = r *^ unitPerp (c ^-^ b)
    -- Split segments.
    ss = (\(a,b) -> [a,b]) $ splitAtParam s 0.5
    subdivided = concatMap (trailSegments . unLoc . offsetSegment epsilon r) ss

    -- Offset with handles scaled based on curvature.
    offset factor = bezier3 (a^*factor) ((b ^-^ c)^*factor ^+^ c ^+^ vc ^-^ va) (c ^+^ vc ^-^ va)
 
    -- We observe a corner.  Subdivide right away.
    go (Finite 0) = subdivided
    -- We have some curvature
    go roc
      | close     = [o]
      | otherwise = subdivided
      where
        -- We want the multiplicative factor that takes us from the original
        -- segment's radius of curvature roc, to roc + r.
        --
        -- r + sr = x * sr
        --
        o = offset $ case roc of
              Infinity  -> 1          -- Do the right thing.
              Finite sr -> 1 + r / sr 

        close = and [epsilon > (magnitude (p o ^+^ va ^-^ p s ^-^ pp s))
                    | t <- [0.25, 0.5, 0.75]
                    , let p = (`atParam` t)
                    , let pp = (r *^) . (`perpAtParam` t)
                    ]


-- > import Diagrams.TwoD.Offset
-- > import Diagrams.Coordinates 
-- >
-- > showExample :: Segment Closed R2 -> Diagram SVG R2
-- > showExample s = pad 1.1 . centerXY $ d # lc blue # lw 0.1 <> d' # lw 0.1
-- >   where
-- >       d  = stroke . fromSegments $ [s]
-- >       d' = mconcat . zipWith lc colors . map stroke . explodeTrail
-- >          $ offsetSegment 0.1 (-1) s
-- >            
-- >       colors = cycle [green, red]
-- > 
-- > cubicOffsetExample :: Diagram SVG R2
-- > cubicOffsetExample = hcat . map showExample $
-- >         [ bezier3 (10 &  0) (  5  & 18) (10 & 20)
-- >         , bezier3 ( 0 & 20) ( 10  & 10) ( 5 & 10)
-- >         , bezier3 (10 & 20) (  0  & 10) (10 &  0)
-- >         , bezier3 (10 & 20) ((-5) & 10) (10 &  0)
-- >         ]
