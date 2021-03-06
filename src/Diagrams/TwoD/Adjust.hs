{-# LANGUAGE ViewPatterns
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Adjust
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A default diagram-adjustment implementation for two-dimensional
-- diagrams, useful for backend implementors.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Adjust
    (
      setDefault2DAttributes
    , adjustDiaSize2D
    , adjustDia2D
    , adjustSize     -- for backwards compatibility
    , requiredScale  -- re-exported for backwards compatibility
    ) where

import Diagrams.Core

import Diagrams.Attributes  (lineWidthA, lineColorA, lineCap
                            , lineJoin, lineMiterLimitA
                            )
import Diagrams.Util        ((#))

import Diagrams.TwoD.Types  (R2, p2)
import Diagrams.TwoD.Size   ( size2D, center2D, SizeSpec2D(..)
                            , requiredScaleT, requiredScale
                            )
import Diagrams.TwoD.Text   (fontSizeA)

import Data.AffineSpace     ((.-.))
import Data.Semigroup

import Data.Default.Class

import Data.Colour.Names    (black)

-- | Set default attributes of a 2D diagram (in case they have not
--   been set):
--
--       * Line width 0.01
--
--       * Line color black
--
--       * Font size 1
--
--       * Line cap LineCapButt
--
--       * line join miter
--
--       * Miter limit 10
setDefault2DAttributes :: Semigroup m => QDiagram b R2 m -> QDiagram b R2 m
setDefault2DAttributes d = d # lineWidthA def # lineColorA def # fontSizeA def
                             # lineCap def # lineJoin def # lineMiterLimitA def

-- | Adjust the size and position of a 2D diagram to fit within the
--   requested size. The first two arguments specify a method for
--   extracting the requested output size from the rendering options,
--   and a way of updating the rendering options with a new (more
--   specific) size.
adjustDiaSize2D :: Monoid' m
                => (Options b R2 -> SizeSpec2D)
                -> (SizeSpec2D -> Options b R2 -> Options b R2)
                -> b -> Options b R2 -> QDiagram b R2 m
                -> (Options b R2, QDiagram b R2 m)
adjustDiaSize2D getSize setSize _ opts d =
  ( case spec of
       Dims _ _ -> opts
       _        -> setSize (uncurry Dims . scale s $ size) opts

  , d # scale s
      # translate tr
  )
  where spec = getSize opts
        size = size2D d
        s    = requiredScale spec size
        finalSz = case spec of
                    Dims w h -> (w,h)
                    _        -> scale s size
        tr = (0.5 *. p2 finalSz) .-. (s *. center2D d)

-- | @adjustDia2D@ provides a useful default implementation of
--   the 'adjustDia' method from the 'Backend' type class.
--
--   As its first two arguments it requires a method for extracting
--   the requested output size from the rendering options, and a way
--   of updating the rendering options with a new (more specific) size.
--
--   It then performs the following adjustments:
--
--   * Set default attributes (see 'setDefault2DAttributes')
--
--   * Freeze the diagram in its final form
--
--   * Scale and translate the diagram to fit within the requested
--     size (see 'adjustDiaSize2D')
--
--   * Also return the actual adjusted size of the diagram.
adjustDia2D :: Monoid' m
            => (Options b R2 -> SizeSpec2D)
            -> (SizeSpec2D -> Options b R2 -> Options b R2)
            -> b -> Options b R2 -> QDiagram b R2 m
            -> (Options b R2, QDiagram b R2 m)
adjustDia2D getSize setSize b opts d
  = adjustDiaSize2D getSize setSize b opts (d # setDefault2DAttributes # freeze)

{-# DEPRECATED adjustSize "Use Diagrams.TwoD.Size.requiredScaleT instead." #-}
-- | Re-export 'requiredScaleT' with the name 'adjustSize' for
--   backwards compatibility.
adjustSize :: SizeSpec2D -> (Double, Double) -> Transformation R2
adjustSize = requiredScaleT
