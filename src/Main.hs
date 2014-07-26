module Main where

import           Codec.Picture
import           Control.Applicative
import           Data.List
import           Data.Maybe
import           Data.Vect
import           Tracer.Shapes

data World = World { objects :: [Object] }

world :: World
world = World { objects =
  [ Ob { color = Color 0 0 255
       , shape = Triangle (Vec3 0 0 10) (Vec3 0 1 10) (Vec3 1 0 10)
       }
  , Ob { color = Color 255 0 0
       , shape = Sphere (Vec3 (-1) (-1) 10) 1
       }
  ]
  }

bgColor :: Color
bgColor = Color 0 0 0

triangleColor :: Color
triangleColor = Color 0 0 255

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
renderPixel x y = PixelRGB8 r g b
  where Color r g b = colorFromRay ray $ objects world
        ray = Ray (Vec3 0 0 0) (Vec3 x y 1)

colorFromRay :: Ray -> [Object] -> Color
colorFromRay r ts = fromMaybe bgColor $ color <$> hit
  where hit = listToMaybe $ allIntersections r ts

allIntersections :: Ray -> [Object] -> [Object]
allIntersections ray ts = map fst $ sortBy depth intersections
  where intersections = [(t, d) | (t, Just d) <- zip ts $ map (intersectWith ray) ts, d > 0]
        depth (_, d) (_, d') = compare d d'
