{-# LANGUAGE GADTs #-}

module Main where

import           Codec.Picture
import           Data.List
import           Data.Maybe
import           Data.Vect
import           Tracer.Shapes
import           Tracer.Lights

data World = World { objects :: [Object], lights :: [AnyLight] }

data Object where
  Ob :: Shape a => { shape :: a
                   , material :: Material
                   } -> Object

instance Shape Object where
  intersectWith r Ob{shape = s} = intersectWith r s
  normal Ob{shape = s} p = normal s p

baseMaterial :: Material
baseMaterial = Material {
  mShininess = 100,
  mSpec = Color 1 1 1,
  mDiff = Color 1 1 1
}

planeMaterial :: Material
planeMaterial = baseMaterial { mSpec = Color 1 1 1 }

world :: World
world = World {
  objects =
      [ Ob { material = planeMaterial
           , shape = Plane (Vec3 0 (-3) 0) (Vec3 0 1 0)
           }
      , Ob { material = planeMaterial
           , shape = Plane (Vec3 0 3 0) (Vec3 0 (-1) 0)
           }
      , Ob { material = planeMaterial
           , shape = Plane (Vec3 0 0 15) (Vec3 0 0 (-1))
           }
      , Ob { material = planeMaterial
           , shape = Plane (Vec3 (-3) 0 0) (Vec3 1 0 0)
           }
      , Ob { material = planeMaterial
           , shape = Plane (Vec3 (3) 0 0) (Vec3 1 0 0)
           }
      , Ob { material = baseMaterial { mDiff = Color 0 0 0.8 }
           , shape = Triangle (Vec3 0 0 10) (Vec3 0 1 10) (Vec3 1 0 10)
           }
      , Ob { material = baseMaterial { mDiff = Color 0.8 0 0 }
           , shape = Sphere (Vec3 (-0.5) (-0.5) 10.5) 1
           }
      ]
  , lights = [AnyLight $ PointLight (Vec3 (-2) 1 10) (Color 2 2 2)]
  }

bgColor :: Color
bgColor = Color 0 0 0

main :: IO ()
main = do
  let image = render 1600 900
  writePng "./result.png" image

render :: Int -> Int -> Image PixelRGB8
render width height = generateImage r width height
  where r x y = renderPixel (toCameraSize width x) $ -(toCameraSize height y)
        toCameraSize range x = camAperture * ((fromIntegral x) - (fromIntegral range)/2)/size
        camAperture = 1
        size = fromIntegral $ max width height

renderPixel :: Float -> Float -> PixelRGB8
renderPixel x y = PixelRGB8 (fence r) (fence g) (fence b)
  where Color r g b = colorFromRay ray $ objects world
        ray = mkRay (Vec3 0 0 0) (Vec3 x y 1)
        fence = round . (*coef) . max 0 . min 1
        coef = fromIntegral (maxBound :: Pixel8)

colorFromRay :: Ray -> [Object] -> Color
colorFromRay r@(Ray o dir) ts = fromMaybe bgColor objectColor
  where
    objectColor = do
      (obj, dist) <- listToMaybe $ allIntersections r ts
      let hitPoint = o &+ (dir &* dist)
          n = normal obj hitPoint
          mat = material obj
      return $ vecSum $ fmap (colorFromMaterial mat dir hitPoint n) (lights world)

allIntersections :: Ray -> [Object] -> [(Object, Float)]
allIntersections ray ts = sortBy depth intersections
  where intersections = [(t, d) | (t, Just d) <- zip ts $ map (intersectWith ray) ts, d > 0]
        depth (_, d) (_, d') = compare d d'
