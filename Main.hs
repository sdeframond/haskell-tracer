module Main where

import           Codec.Picture
import           Data.List
import           Data.Vect
import           Data.Word

type Point = Vec3
type Direction = Vec3

data World = World [Triangle]

data Plane = Plane Point Direction

data Triangle = Triangle Point Point Point

data Color = Color Word8 Word8 Word8

data Ray = Ray Point Direction

world :: World
world = World [ Triangle (Vec3 0 0 10) (Vec3 0 1 10) (Vec3 1 0 10)]

bgColor :: Color
bgColor = Color 0 0 0

triangleColor :: Color
triangleColor = Color 0 0 255

main :: IO ()
main = do
  let image = render 500 500 world
  writePng "./result.png" image

render :: Int -> Int -> World -> Image PixelRGB8
render width height world = generateImage r width height
  where r x y = renderPixel (normalize width x) $ -(normalize height y)
        normalize range x = ((fromIntegral x) - (fromIntegral range)/2)/size
        size = fromIntegral $ max width height

renderPixel :: Float -> Float -> PixelRGB8
renderPixel x y = PixelRGB8 r g b
  where Color r g b = colorFromRay ray world
        ray = Ray (Vec3 0 0 0) (Vec3 x y 1)

colorFromRay :: Ray -> World -> Color
colorFromRay r (World ts) = color hit
  where color Nothing = bgColor
        color _ = triangleColor
        hit = first $ allIntersections r ts
        first [] = Nothing
        first (x:xs) = Just x

intersectWith :: Ray -> Triangle -> Maybe (Triangle, Float)
intersectWith ray@(Ray origin dir) t = do
  distance <- intersectWithPlane ray $ planeFromTriangle t
  let inside = withinTriangle t ((dir &* distance) &+ origin)
  if inside then return (t, distance) else Nothing

withinTriangle :: Triangle -> Point -> Bool
withinTriangle (Triangle p1 p2 p3) p = s >= 0 && t >= 0 && s+t <= 1
  where u = p2 &- p1
        v = p3 &- p1
        w = p &- p1
        uv = u &. v
        wv = w &. v
        vv = v &. v
        wu = w &. u
        uu = u &. u
        s = (uv*wv - vv*wu)/d
        t = (uv*wu - uu*wv)/d
        d = uv**2 - uu*vv

planeFromTriangle :: Triangle -> Plane
planeFromTriangle (Triangle p1 p2 p3) = Plane p1 n
  where n = normalize ((p2 &- p1) &^ (p3 &- p1))

intersectWithPlane :: Ray -> Plane -> Maybe Float
intersectWithPlane (Ray orig dir) (Plane point n)
  | parallel = Nothing
  | otherwise = Just $ (n &. vecToPoint)/(n &. dir)
  where vecToPoint = point &- orig
        parallel = n &. dir == 0

allIntersections :: Ray -> [Triangle] -> [Triangle]
allIntersections ray ts = map fst $ sortBy distance intersections
  where intersections = [(t, d) | Just (t, d) <- map (intersectWith ray) ts]
        distance (_, d) (_, d') = compare d d'
