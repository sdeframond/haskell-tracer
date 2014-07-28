{-# LANGUAGE GADTs #-}

module Tracer.Lights ( colorFromMaterial
                     , AnyLight(..)
                     , PointLight(..)
                     , mkDirLight
                     , Color(..)
                     , LightSource(..)
                     , Material(..)
                     ) where

import           Data.List
import           Data.Maybe
import           Data.Vect
import           Tracer.Shapes

type Point = Vec3
type Direction = Vec3

data Material = Material { mShininess :: Float
                         , mSpec :: Color
                         , mDiff:: Color
                         , mAmb:: Float
                         }
                         deriving Eq

data Color = Color Float Float Float deriving Eq

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

colorFromMaterial :: (LightSource a, Shape b)
  => Material -> Direction -> Point -> Direction -> b -> [b] -> a -> Color
colorFromMaterial m rayDir p n sh shapes l = if lIsVisible l p sh shapes
  then d &+ s &+ a
  else a
  where d = (mDiff m) &! (i &* (max 0 $ n &. neg dir))
        s = (mSpec m) &! (i &* f**shiny)
        a = (mAmb m) *& ((mDiff m) &! i)
        i = lIntensity l p
        dir = lDirection l p
        shiny = mShininess m
        f = max 0 $ r &. rayDir
        r = (2 * (dir &. n)) *& n &- dir

class LightSource a where
  lIntensity :: a -> Point -> Color
  lDirection :: a -> Point -> Direction
  lIsVisible :: Shape b => a -> Point -> b -> [b] -> Bool

data PointLight = PointLight Point Color
instance LightSource PointLight where
  lIntensity (PointLight o c) p = c &* (1/(1+dist)**2)
    where dist = norm $ p &- o
  lDirection (PointLight o _) p = normalize $ p &- o
  lIsVisible (PointLight o _) p ob obs =
    case rayIntersection ray ob obs of
      Just d -> d > norm lightDir
      Nothing -> False
    where
      ray = mkRay p lightDir
      lightDir = o &- p

data DirectionalLight = DirLight Direction Color
instance LightSource DirectionalLight where
  lIntensity (DirLight _ c) _ = c
  lDirection (DirLight d _) _ = d
  lIsVisible (DirLight d _) p sh shapes = (Nothing == rayIntersection ray sh shapes)
    where ray = mkRay p (neg d)

rayIntersection :: Shape a => Ray -> a -> [a] -> Maybe Float
rayIntersection ray sh shapes = listToMaybe $ sort intersections
  where intersections = [d | Just d <- map (intersectWith ray) otherShapes, d > 0]
        otherShapes = filter (/=sh) shapes

mkDirLight :: Direction -> Color -> DirectionalLight
mkDirLight dir c = DirLight (normalize dir) c

data AnyLight where
  AnyLight :: LightSource l => l -> AnyLight
instance LightSource AnyLight where
  lIntensity (AnyLight l) p = lIntensity l p
  lDirection (AnyLight l) p = lDirection l p
  lIsVisible (AnyLight l) p shapes = lIsVisible l p shapes
