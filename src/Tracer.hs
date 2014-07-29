{-# LANGUAGE GADTs #-}

module Tracer ( colorFromRay
              , rayIntersection
              , linearPerspective
              , sphericalPerspective
              , mkRenderer
              , Scene(..)
              , Object(..)
              , Color(..)
              ) where

import           Data.List
import           Data.Maybe
import           Data.Typeable
import           Data.Vect
import           Tracer.Color
import           Tracer.Lights
import           Tracer.Shapes

data Scene = Scene {
  objects :: [Object],
  lights :: [AnyLight],
  bgColor :: Color
}

data Object where
  Ob :: (Shape a, Typeable a) => {
    shape :: a,
    material :: Material
  } -> Object

instance Eq Object where
  Ob{shape = s1} == Ob{shape = s2} = Just s1 == cast s2

instance Shape Object where
  intersectWith r Ob{shape = s} = intersectWith r s
  normal Ob{shape = s} p = normal s p

colorFromRay :: Ray -> Scene -> Color
colorFromRay r@(Ray o dir) scene = fromMaybe (bgColor scene) objectColor
  where
    objectColor = do
      (obj, dist) <- rayIntersection r (objects scene)
      let hitPoint = o &+ (dir &* dist)
          n = normal obj hitPoint
          mat = material obj
          colorFromLight = (\l ->
            let d = diffuseColor mat i n lDir
                s = specularColor mat i n lDir dir
                a = ambientColor mat i
                i = lIntensity l hitPoint
                lDir = lDirection l hitPoint
            in if lIsVisible l hitPoint obj (objects scene)
               then d &+ s &+ a
               else a
            )
      return $ vecSum $ fmap colorFromLight (lights scene)

rayIntersection :: Ray -> [Object] -> Maybe (Object, Float)
rayIntersection ray ts = listToMaybe $ sortBy depth intersections
  where intersections = [(t, d) | (t, Just d) <- zip ts $ map (intersectWith ray) ts, d > 0]
        depth (_, d) (_, d') = compare d d'

type Perspective = Float -> Float -> Ray
type Renderer = Int -> Int -> Color

linearPerspective :: Perspective
linearPerspective x y = mkRay (Vec3 0 0 0) (Vec3 x y 1)

sphericalPerspective :: Perspective
sphericalPerspective x y = mkRayLongLat (Vec3 0 0 0) x y

mkRenderer :: Scene -> Int -> Int -> Perspective -> Float -> Renderer
mkRenderer scene width height perspective aperture x y = colorFromRay ray scene
  where ray = perspective (toCameraSize width x) $ -(toCameraSize height y)
        toCameraSize range x = aperture * ((fromIntegral x) - (fromIntegral range)/2)/size
        size = fromIntegral $ max width height
