{-# LANGUAGE GADTs #-}

module PathTracer ( linearPerspective
                  , sphericalPerspective
                  , mkRenderer
                  , demoScene
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
import           System.Random

data Scene = Scene {
  objects :: [Object],
  lights :: [AnyLight],
  bgLight :: Direction -> Color
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

type Point = Vec3
type Direction = Vec3

rayIntersection :: Ray -> [Object] -> Maybe (Object, Float)
rayIntersection ray ts = listToMaybe $ sortBy depth intersections
  where intersections = [(t, d) | (t, Just d) <- zip ts $ map (intersectWith ray) ts, d > 0]
        depth (_, d) (_, d') = compare d d'

tracePath :: Scene -> StdGen -> Int -> Ray -> Color
tracePath scene rndGen ttl ray@(Ray o dir)
  | ttl <= 0 = Color 0 0 0
  | otherwise = fromMaybe (bgLight scene dir) $ do
  (obj, dist) <- rayIntersection ray (objects scene)
  let hitPoint = o &+ (dir &* dist)
      n = normal obj hitPoint
      mat = material obj
      numberOfRays = 10
      (rndGen', rndGen'') = split rndGen
      newRays = [randomRayFromMaterial mat hitPoint n x y | (x, y) <- take numberOfRays $ pairs $ randoms rndGen']
      pairs (x:y:xs) = (x,y) : pairs xs
      bdrfs = map (bdrf mat dir n) newRays
      receivedLights = map (tracePath scene rndGen'' (ttl-1)) newRays
      reflectedLight = average $ zipWith (&!) bdrfs receivedLights
      average xs = vecSum xs &* (1 / genericLength xs)
      emittance m = Color 0 0 0
  return ((emittance mat) &+ (reflectedLight))

randomRayFromMaterial :: Material -> Point -> Direction -> Float -> Float -> Ray
randomRayFromMaterial mat point n x y = if dir &. n < 0 then Ray o (neg dir) else ray
  where ray@(Ray o dir) = mkRayLongLat point x y

bdrf :: Material -> Direction -> Direction -> Ray -> Color
bdrf mat n inDir (Ray _ outDir) = (mDiff mat) &* (n &. outDir)

type Perspective = Float -> Float -> Ray
type Renderer = Int -> Int -> Color

linearPerspective :: Perspective
linearPerspective x y = mkRay (Vec3 0 0 0) (Vec3 x y 1)

sphericalPerspective :: Perspective
sphericalPerspective x y = mkRayLongLat (Vec3 0 0 0) x y

mkRenderer :: Scene -> StdGen -> Int -> Int -> Perspective -> Float -> Renderer
mkRenderer scene rndGen width height perspective aperture x y = tracePath scene rndGen ttl ray
  where ray = perspective (toCameraSize width x) $ -(toCameraSize height y)
        ttl = 3
        toCameraSize range value = aperture * ((fromIntegral value) - (fromIntegral range)/2)/size
        size = fromIntegral $ max width height

---------------------------------------------------

baseMaterial :: Material
baseMaterial = Material {
  mShininess = 100,
  mSpec = Color 0.9 0.9 0.9,
  mDiff = Color 0.9 0.9 0.9,
  mAmb = 0.1
}

planeMaterial :: Material
planeMaterial = baseMaterial { mSpec = Color 1 1 1 }

demoScene :: Scene
demoScene = Scene {
  bgLight = (\ dir -> (dir &. (normalize $ Vec3 1 1 1))**4 *& (Color 0.3 0.3 0.3))
  , objects =
      [ Ob { material = planeMaterial
           , shape = Plane (Vec3 0 (-1) 0) (Vec3 0 1 0)
           }
      , Ob { material = baseMaterial { mDiff = Color 0 0 0.8 }
           , shape = Triangle (Vec3 0 0 4) (Vec3 0 1 4) (Vec3 1 0 4)
           }
      , Ob { material = baseMaterial { mDiff = Color 0.8 0 0 }
           , shape = Sphere (Vec3 (-0.5) (0) 4.5) 1
           }
      ]
  , lights =  [ AnyLight $ mkDirLight (Vec3 0 (-1) 0) (Color 0.3 0.3 0.3)
              , AnyLight $ PointLight (Vec3 (-2) 1 10) (Color 3 3 3)
              , AnyLight $ PointLight (Vec3 2 0 10) (Color 3 1.5 0)
              ]
  }
