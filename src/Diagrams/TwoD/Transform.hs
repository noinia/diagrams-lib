{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Transform
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Transformations specific to two dimensions, with a few generic
-- transformations (uniform scaling, translation) also re-exported for
-- convenience.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Transform
       (
         -- * Rotation
         rotation, rotate, rotateBy

       , rotationAbout, rotateAbout

         -- * Scaling
       , scalingX, scaleX
       , scalingY, scaleY
       , scaling, scale

       , scaleToX, scaleToY
       , scaleUToX, scaleUToY

         -- * Translation
       , translationX, translateX
       , translationY, translateY
       , translation, translate

         -- * Reflection
       , reflectionX, reflectX
       , reflectionY, reflectY
       , reflectionAbout, reflectAbout

         -- * Shears
       , shearingX, shearX
       , shearingY, shearY

         -- * component-wise
       , onBasis
       ) where

import           Diagrams.Core
import qualified Diagrams.Core.Transform as T

import           Diagrams.Coordinates
import           Diagrams.Transform
import           Diagrams.TwoD.Size      (height, width)
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector    (direction)

import           Data.AffineSpace
import           Data.Semigroup

-- Rotation ------------------------------------------------

-- | Shorthand that specifies b is the NumericType of a, and b is floating
type BasicFloatingType a b = (NumericType a ~ b, Floating b)


-- | Create a transformation which performs a rotation about the local
--   origin by the given angle.  See also 'rotate'.
rotation :: (Angle a, BasicFloatingType a b) => a -> T2 b
rotation ang = fromLinear r (linv r)
  where
    r            = rot theta <-> rot (-theta)
    Rad theta    = convertAngle ang
    rot th (coords -> x :& y) = (cos th * x - sin th * y) & (sin th * x + cos th * y)

-- | Rotate about the local origin by the given angle. Positive angles
--   correspond to counterclockwise rotation, negative to
--   clockwise. The angle can be expressed using any type which is an
--   instance of 'Angle'.  For example, @rotate (1\/4 ::
--   'Turn')@, @rotate (tau\/4 :: 'Rad')@, and @rotate (90 ::
--   'Deg')@ all represent the same transformation, namely, a
--   counterclockwise rotation by a right angle.  To rotate about some
--   point other than the local origin, see 'rotateAbout'.
--
--   Note that writing @rotate (1\/4)@, with no type annotation, will
--   yield an error since GHC cannot figure out which sort of angle
--   you want to use.  In this common situation you can use
--   'rotateBy', which is specialized to take a 'Turn' argument.
rotate :: (Transformable t, V t ~ V2 b, Angle a, BasicFloatingType a b) => a -> t -> t
rotate = transform . rotation

-- | A synonym for 'rotate', specialized to only work with
--   @Turn@ arguments; it can be more convenient to write
--   @rotateBy (1\/4)@ than @'rotate' (1\/4 :: 'Turn')@.
rotateBy :: (Transformable t, V t ~ V2 b, Floating b) => Turn b -> t -> t
rotateBy = transform . rotation

-- | @rotationAbout p@ is a rotation about the point @p@ (instead of
--   around the local origin).
rotationAbout :: (Angle a, BasicFloatingType a b) => P2 b -> a -> T2 b
rotationAbout p angle = conjugate (translation (origin .-. p)) (rotation angle)

-- | @rotateAbout p@ is like 'rotate', except it rotates around the
--   point @p@ instead of around the local origin.
rotateAbout :: (Transformable t, V t ~ V2 b, Angle a, BasicFloatingType a b) => P2 b -> a -> t -> t
rotateAbout p angle = rotate angle `under` translation (origin .-. p)

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the x (horizontal) direction.
scalingX :: Fractional b => b -> T2 b
scalingX c = fromLinear s s
  where s = (\v -> let (x,y) = unv2 v in V2 (x*c) y)
            <->
            (\v -> let (x,y) = unv2 v in V2 (x/c) y)

-- | Scale a diagram by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use 'scale'.
scaleX :: (Transformable t, V t ~ V2 b, Fractional b) => b -> t -> t
scaleX = transform . scalingX

-- | Construct a transformation which scales by the given factor in
--   the y (vertical) direction.
scalingY :: Fractional b => b -> T2 b
scalingY c = fromLinear s s
  where s = (\v -> let (x,y) = unv2 v in V2 x (y*c))
            <->
            (\v -> let (x,y) = unv2 v in V2 x (y/c))

-- | Scale a diagram by the given factor in the y (vertical)
--   direction.  To scale uniformly, use 'scale'.
scaleY :: (Transformable t, V t ~ V2 b, Fractional b) => b -> t -> t
scaleY = transform . scalingY

-- | @scaleToX w@ scales a diagram in the x (horizontal) direction by
--   whatever factor required to make its width @w@.  @scaleToX@
--   should not be applied to diagrams with a width of 0, such as
--   'vrule'.
scaleToX :: (Enveloped t, Transformable t, V t ~ V2 b, Fractional b) => b -> t -> t
scaleToX w d = scaleX (w / width d) d

-- | @scaleToY h@ scales a diagram in the y (vertical) direction by
--   whatever factor required to make its height @h@.  @scaleToY@
--   should not be applied to diagrams with a height of 0, such as
--   'hrule'.
scaleToY :: (Enveloped t, Transformable t, V t ~ V2 b, Fractional b) => b -> t -> t
scaleToY h d = scaleY (h / height d) d

-- | @scaleUToX w@ scales a diagram /uniformly/ by whatever factor
--   required to make its width @w@.  @scaleUToX@ should not be
--   applied to diagrams with a width of 0, such as 'vrule'.
scaleUToX :: (Enveloped t, Transformable t, V t ~ V2 b, Fractional b) => b -> t -> t
scaleUToX w d = scale (w / width d) d

-- | @scaleUToY h@ scales a diagram /uniformly/ by whatever factor
--   required to make its height @h@.  @scaleUToY@ should not be applied
--   to diagrams with a height of 0, such as 'hrule'.
scaleUToY :: (Enveloped t, Transformable t, V t ~ V2 b, Fractional b) => b -> t -> t
scaleUToY h d = scale (h / height d) d

-- Translation ---------------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the x (horizontal) direction.
translationX :: Num b => b -> T2 b
translationX x = translation (x & 0)

-- | Translate a diagram by the given distance in the x (horizontal)
--   direction.
translateX :: (Transformable t, V t ~ V2 b, Num b) => b -> t -> t
translateX = transform . translationX

-- | Construct a transformation which translates by the given distance
--   in the y (vertical) direction.
translationY :: Num b => b -> T2 b
translationY y = translation (0 & y)

-- | Translate a diagram by the given distance in the y (vertical)
--   direction.
translateY :: (Transformable t, V t ~ V2 b, Num b) => b -> t -> t
translateY = transform . translationY

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips a diagram from left to
--   right, i.e. sends the point (x,y) to (-x,y).
reflectionX :: Fractional b => T2 b
reflectionX = scalingX (-1)

-- | Flip a diagram from left to right, i.e. send the point (x,y) to
--   (-x,y).
reflectX :: (Transformable t, V t ~ V2 b, Fractional b) => t -> t
reflectX = transform reflectionX

-- | Construct a transformation which flips a diagram from top to
--   bottom, i.e. sends the point (x,y) to (x,-y).
reflectionY :: Fractional b => T2 b
reflectionY = scalingY (-1)

-- | Flip a diagram from top to bottom, i.e. send the point (x,y) to
--   (x,-y).
reflectY :: (Transformable t, V t ~ V2 b, Fractional b) => t -> t
reflectY = transform reflectionY

-- | @reflectionAbout p v@ is a reflection in the line determined by
--   the point @p@ and vector @v@.
reflectionAbout :: RealFloat b => P2 b -> V2 b -> T2 b
reflectionAbout p v = reflectionAbout' p (-direction v)
    where
      reflectionAbout' :: Floating b => P2 b -> Rad b -> T2 b
      reflectionAbout' p dir = conjugate (rotation dir <> translation (origin .-. p))
                               reflectionY

-- | @reflectAbout p v@ reflects a diagram in the line determined by
--   the point @p@ and the vector @v@.
reflectAbout :: (Transformable t, V t ~ (V2 b), RealFloat b) => P2 b -> (V2 b) -> t -> t
reflectAbout p v = transform (reflectionAbout p v)

-- Shears --------------------------------------------------

-- | @shearingX d@ is the linear transformation which is the identity on
--   y coordinates and sends @(0,1)@ to @(d,1)@.
shearingX :: Num b => b -> T2 b
shearingX d = fromLinear (sh d  <-> sh (-d))
                         (sh' d <-> sh' (-d))
  where sh  k (unv2 -> (x,y)) = V2 (x+k*y) y
        sh' k                 = swap . sh k . swap
        swap (unv2 -> (x,y))  = V2 y x

-- | @shearX d@ performs a shear in the x-direction which sends
--   @(0,1)@ to @(d,1)@.
shearX :: (Transformable t, V t ~ V2 b, Num b) => b -> t -> t
shearX = transform . shearingX

-- | @shearingY d@ is the linear transformation which is the identity on
--   x coordinates and sends @(1,0)@ to @(1,d)@.
shearingY :: Num b => b -> T2 b
shearingY d = fromLinear (sh d  <-> sh (-d))
                         (sh' d <-> sh' (-d))
  where sh  k (unv2 -> (x,y)) = V2 x (y+k*x)
        sh' k                 = swap . sh k . swap
        swap (unv2 -> (x,y))  = V2 y x

-- | @shearY d@ performs a shear in the y-direction which sends
--   @(1,0)@ to @(1,d)@.
shearY :: (Transformable t, V t ~ V2 b, Num b) => b -> t -> t
shearY = transform . shearingY

-- | Get the matrix equivalent of the linear transform,
--   (as a pair of columns) and the translation vector.  This
--   is mostly useful for implementing backends.
onBasis :: Num b => T2 b -> ((V2 b, V2 b), V2 b)
onBasis t = ((x, y), v)
  where ((x:y:[]), v) = T.onBasis t
