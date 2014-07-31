module Main where

import           Codec.Picture
import           PathTracer
import           System.Random

main :: IO ()
main = do
  rndGen <- newStdGen
  let image = render rndGen 500 300
  writePng "./result.png" image

render :: StdGen -> Int -> Int -> Image PixelRGB8
render rndGen width height = snd $ generateFoldImage r rndGen width height
  where
    r rg x y = (rg', pixel)
      where pixel = colorToPixelRGB8 $ renderer rg'' x y
            (rg', rg'') = split rg
    renderer = mkRenderer demoScene width height sphericalPerspective 1

colorToPixelRGB8 :: Color -> PixelRGB8
colorToPixelRGB8 (Color r g b) = PixelRGB8 (fence r) (fence g) (fence b)
  where fence = round . (*coef) . max 0 . min 1
        coef = fromIntegral (maxBound :: Pixel8)