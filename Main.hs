{-# LANGUAGE GADTs #-}

module Main where

import           Codec.Picture
import           Data.List
import           Data.Vect
import           Data.Word

type Point = Vec3
type Direction = Vec3

data Color = Color Word8 Word8 Word8

data Ray = Ray Point Direction

data World = World { wTriangles :: [Triangle], wSpheres :: [Sphere] }

data Plane = Plane Point Direction

data Triangle = Triangle Point Point Point
data Sphere = Sphere Point Float

data AnyShape where
  AnyShape :: Shape a => a -> AnyShape

class Shape a where
  intersectWith :: Ray -> a -> Maybe Float

instance Shape Plane where
  intersectWith (Ray orig dir) (Plane point n)
    | parallel = Nothing
    | otherwise = Just distance
    where vecToPoint = point &- orig
          parallel = n &. dir == 0
          distance = (n &. vecToPoint)/(n &. dir)

instance Shape Triangle where
  intersectWith ray@(Ray origin dir) t = do
    distance <- intersectWith ray $ planeFromTriangle t
    let intersection = (dir &* distance) &+ origin
        isInside = withinTriangle t intersection
    if isInside then return distance else Nothing
    where
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

instance Shape Sphere where
  intersectWith r s = Nothing

instance Shape AnyShape where
  intersectWith r (AnyShape a) = intersectWith r a

world :: World
world = World { wTriangles = [Triangle (Vec3 0 0 10) (Vec3 0 1 10) (Vec3 1 0 10)]
              , wSpheres = [Sphere (Vec3 (-1) 0 10) 1]
              }

shapes :: World -> [AnyShape]
shapes w = map AnyShape (wTriangles w) ++ map AnyShape (wSpheres w)

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
  where r x y = renderPixel (normalize width x) $ -(normalize height y)
        normalize range x = ((fromIntegral x) - (fromIntegral range)/2)/size
        size = fromIntegral $ max width height

renderPixel :: Float -> Float -> PixelRGB8
renderPixel x y = PixelRGB8 r g b
  where Color r g b = colorFromRay ray $ shapes world
        ray = Ray (Vec3 0 0 0) (Vec3 x y 1)

colorFromRay :: Ray -> [AnyShape] -> Color
colorFromRay r ts = color hit
  where color Nothing = bgColor
        color _ = triangleColor
        hit = first $ allIntersections r ts
        first [] = Nothing
        first (x:xs) = Just x

allIntersections :: Ray -> [AnyShape] -> [AnyShape]
allIntersections ray ts = map fst $ sortBy distance intersections
  where intersections = [(t, d) | (t, Just d) <- zip ts $ map (intersectWith ray) ts]
        distance (_, d) (_, d') = compare d d'
