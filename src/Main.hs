module Main where

import           Codec.Picture
import           Tracer
import           Tracer.DemoScene

main :: IO ()
main = do
  let image = render 1600 900
  writePng "./result.png" image

render :: Int -> Int -> Image PixelRGB8
render width height = generateImage r width height
  where r x y = colorToPixelRGB8 $ renderer x y
        renderer = mkRenderer demoScene width height sphericalPerspective 1

colorToPixelRGB8 :: Color -> PixelRGB8
colorToPixelRGB8 (Color r g b) = PixelRGB8 (fence r) (fence g) (fence b)
  where fence = round . (*coef) . max 0 . min 1
        coef = fromIntegral (maxBound :: Pixel8)
