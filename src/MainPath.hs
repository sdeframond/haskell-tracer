module Main where

import           Codec.Picture
import           Control.Parallel.Strategies
import           PathTracer
import           System.Random

main :: IO ()
main = do
  rndGen <- newStdGen
  let image = render rndGen 500 500
  writePng "./result.png" image

render :: StdGen -> Int -> Int -> Image PixelRGB8
--render width height = generateImage r width height
render rndGen width height = snd $ generateFoldImage (\(p:ps) _ _ -> (ps, p)) picture width height
  where r x y = colorToPixelRGB8 $ renderer x y
        pic = [r x y | y <- [0..height-1], x <- [0..width-1]]
        picture = runEval $ parBuffer 500 rseq pic
        renderer = mkRenderer demoScene rndGen width height sphericalPerspective 1

colorToPixelRGB8 :: Color -> PixelRGB8
colorToPixelRGB8 (Color r g b) = PixelRGB8 (fence r) (fence g) (fence b)
  where fence = round . (*coef) . max 0 . min 1
        coef = fromIntegral (maxBound :: Pixel8)