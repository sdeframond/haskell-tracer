{-# LANGUAGE GADTs #-}

module Tracer.Lights ( diffuseColor
                     , specularColor
                     , ambientColor
                     , AnyLight(..)
                     , PointLight(..)
                     , mkDirLight
                     , LightSource(..)
                     , Material(..)
                     ) where

import           Data.List
import           Data.Maybe
import           Data.Vect
import           Tracer.Color
import           Tracer.Shapes

type Point = Vec3
type Direction = Vec3

data Material = Material { mShininess :: Float
                         , mSpec      :: Color
                         , mDiff      :: Color
                         , mAmb       :: Float
                         }
                         deriving Eq

ambientColor :: Material -> Color -> Color
ambientColor m i = (mAmb m) *& ((mDiff m) &! i)

diffuseColor :: Material -> Color -> Direction -> Direction -> Color
diffuseColor m i n dir = (mDiff m) &! (i &* (max 0 $ n &. neg dir))

specularColor :: Material -> Color -> Direction -> Direction -> Direction -> Color
specularColor m i n dir rayDir = (mSpec m) &! (i &* f**shiny)
  where shiny = mShininess m
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
