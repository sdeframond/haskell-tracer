{-# LANGUAGE GADTs #-}

module Tracer.Lights ( colorFromMaterial
                     , AnyLight(..)
                     , PointLight(..)
                     , Color(..)
                     , LightSource(..)
                     , Material(..)
                     ) where

import           Data.Vect

type Point = Vec3
type Direction = Vec3

data Material = Material { mShininess :: Float
                         , mSpec :: Color
                         , mDiff:: Color
                         }

data Color = Color Float Float Float

instance Pointwise Color where
  pointwise (Color r g b) (Color r' g' b') = Color (r*r') (g*g') (b*b')

instance AbelianGroup Color where
  (&+) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)
  (&-) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1-r2) (g1-g2) (b1-b2)
  neg  (Color r g b)                     = Color (-r) (-g) (-b)
  zero = Color 0 0 0

instance Vector Color where
  scalarMul s (Color r g b) = Color (s*r) (s*g) (s*b)
  mapVec    f (Color r g b) = Color (f r) (f g) (f b)

colorFromMaterial :: LightSource a => Material -> Direction -> Point -> Direction -> a -> Color
colorFromMaterial m v p n l = d &+ s &+ a
  where d = (mDiff m) &! (diffuseAt p n l)
        s = (mSpec m) &! (specularAt p n l v $ mShininess m)
        a = (mDiff m) &! (lIntensity l p)

specularAt :: LightSource a => Point -> Direction -> a -> Direction -> Float -> Color
specularAt p n l v s = (lIntensity l p) &* factor
  where factor = if f > 0 then f ** s else 0
        f = r &. v
        r = (2 * (dir &. n)) *& n &- dir
        dir = lDirection l p

diffuseAt :: LightSource a => Point -> Direction -> a -> Color
diffuseAt p n l = (lIntensity l p) &* factor
  where factor =  max 0 $ n &. (neg $ lDirection l p)

class LightSource a where
  lIntensity :: a -> Point -> Color
  lDirection :: a -> Point -> Direction

data PointLight = PointLight Point Color
instance LightSource PointLight where
  lIntensity (PointLight o c) p = c &* (1/dist**2)
    where dist = norm $ p &- o
  lDirection (PointLight o _) p = normalize $ p &- o

data AnyLight where
  AnyLight :: LightSource l => l -> AnyLight
instance LightSource AnyLight where
  lIntensity (AnyLight l) p = lIntensity l p
  lDirection (AnyLight l) p = lDirection l p
