module Tracer.Shapes ( mkRay
                     , Ray(Ray)
                     , Shape(..)
                     , Triangle(..)
                     , Sphere(..)
                     , Plane(..)
                   ) where

import           Data.Vect

type Point = Vec3
type Direction = Vec3

data Ray = Ray Point Direction

mkRay :: Point -> Direction -> Ray
mkRay p d = Ray p (normalize d)

class Shape a where
  intersectWith :: Ray -> a -> Maybe Float
  normal :: a -> Point -> Direction

data Plane = Plane Point Direction

instance Shape Plane where
  intersectWith (Ray orig dir) (Plane point n)
    | parallel = Nothing
    | otherwise = Just d
    where vecToPoint = point &- orig
          parallel = n &. dir == 0
          d = (n &. vecToPoint)/(n &. dir)
  normal (Plane _ dir) _ = dir

data Triangle = Triangle Point Point Point
instance Shape Triangle where
  intersectWith ray@(Ray origin dir) t = do
    d <- intersectWith ray $ planeFromTriangle t
    let intersection = (dir &* d) &+ origin
        isInside = withinTriangle t intersection
    if isInside then return d else Nothing
  normal t p = normal (planeFromTriangle t) p

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
  where n = normalize $ (p2 &- p1) &^ (p3 &- p1)

data Sphere = Sphere Point Float

instance Shape Sphere where
  intersectWith (Ray ro dir) (Sphere so r)
    | d2 > (r**2) = Nothing
    | otherwise = Just $ (vecToCenter &. dir) - (sqrt (r**2 - d2))
    where
      vecToCenter = so &- ro
      projVec = dir &* (vecToCenter &. dir) &- vecToCenter
      d2 = projVec &. projVec
  normal (Sphere o _) p = normalize $ p &- o