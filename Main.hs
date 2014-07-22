module Main where

import           Codec.Picture

main :: IO ()
main = writePng "./result.png" image
  where
    image = generateImage render 500 500

    --render :: Int -> Int -> a
    render x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
