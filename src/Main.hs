module Main where

import           Codec.Picture
import           Data.List
import           Data.Maybe
import           Data.Vect
import           Tracer.Shapes

data World = World { objects :: [Object], lights :: [Light] }

world :: World
world = World {
  objects =
      [ Ob { color = Color 1 1 1
           , shape = Plane (Vec3 0 (-1) 0) (Vec3 0 1 0)
           }
      , Ob { color = Color 0 0 1
           , shape = Triangle (Vec3 0 0 10) (Vec3 0 1 10) (Vec3 1 0 10)
           }
      , Ob { color = Color 1 0 0
           , shape = Sphere (Vec3 (-0.5) (-0.5) 10.5) 1
           }
      ]
  , lights = [Light (Vec3 (-3) 2 8) (Color 2 2 2)]
  }

bgColor :: Color
bgColor = Color 0 0 0

ambColor :: Color
ambColor = Color 0.1 0.1 0.1

main :: IO ()
main = do
  let image = render 1600 900
  writePng "./result.png" image

render :: Int -> Int -> Image PixelRGB8
render width height = generateImage r width height
  where r x y = renderPixel (toCameraSize width x) $ -(toCameraSize height y)
        toCameraSize range x = ((fromIntegral x) - (fromIntegral range)/2)/size
        size = fromIntegral $ max width height

renderPixel :: Float -> Float -> PixelRGB8
renderPixel x y = PixelRGB8 (fence r) (fence g) (fence b)
  where Color r g b = colorFromRay ray $ objects world
        ray = Ray (Vec3 0 0 0) (Vec3 x y 1)
        fence = round . (*255) . max 0 . min 1

colorFromRay :: Ray -> [Object] -> Color
colorFromRay r@(Ray o dir) ts = fromMaybe bgColor objectColor
  where
    objectColor = do
      (obj, dist) <- hit
      let c = color obj
          hitPoint = o &+ (dir &* dist)
          n = normal obj hitPoint
          a = ambColor &! c
          d = c &! (vecSum $ fmap (diffuseAt hitPoint n) (lights world))
          s = zero
      return $ vecSum [a, d, s]
    hit = listToMaybe $ allIntersections r ts

diffuseAt :: Vec3 -> Vec3 -> Light -> Color
diffuseAt p n (Light pl c) = c &* factor
  where factor =  max 0 $ (n &. vecToLight) / dist**2
        dist = norm vecToLight
        vecToLight = pl &- p

allIntersections :: Ray -> [Object] -> [(Object, Float)]
allIntersections ray ts = sortBy depth intersections
  where intersections = [(t, d) | (t, Just d) <- zip ts $ map (intersectWith ray) ts, d > 0]
        depth (_, d) (_, d') = compare d d'
