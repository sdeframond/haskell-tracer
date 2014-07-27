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
  where d = (mDiff m) &! (i &* (max 0 $ n &. neg dir))
        s = (mSpec m) &! (i &* f**shiny)
        a = (mDiff m) &! i
        i = lIntensity l p
        dir = lDirection l p
        shiny = mShininess m
        f = max 0 $ r &. v
        r = (2 * (dir &. n)) *& n &- dir

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
