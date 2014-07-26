{-# LANGUAGE GADTs #-}

module Tracer.Shapes ( Point
                     , Direction
                     , Color(..)
                     , Ray(..)
                     , Shape(..)
                     , Triangle(..)
                     , Sphere(..)
                     , Object(..)
                   ) where

import           Data.Vect
import           Data.Word

type Point = Vec3
type Direction = Vec3

data Color = Color Word8 Word8 Word8

data Ray = Ray Point Direction

class Shape a where
  intersectWith :: Ray -> a -> Maybe Float

data Plane = Plane Point Direction

instance Shape Plane where
  intersectWith (Ray orig dir) (Plane point n)
    | parallel = Nothing
    | otherwise = Just d
    where vecToPoint = point &- orig
          parallel = n &. dir == 0
          d = (n &. vecToPoint)/(n &. dir)

data Triangle = Triangle Point Point Point
instance Shape Triangle where
  intersectWith ray@(Ray origin dir) t = do
    d <- intersectWith ray $ planeFromTriangle t
    let intersection = (dir &* d) &+ origin
        isInside = withinTriangle t intersection
    if isInside then return d else Nothing

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

data Sphere = Sphere Point Float

instance Shape Sphere where
  intersectWith (Ray ro dir) (Sphere so r)
    | d2 > (r**2) = Nothing
    | otherwise = Just $ (vecToCenter &. dir) - (sqrt (r**2 - d2))
    where
      vecToCenter = so &- ro
      projVec = dir &* (vecToCenter &. dir) &- vecToCenter
      d2 = projVec &. projVec

data Object where
  Ob :: Shape a => { shape :: a
                   , color :: Color
                   } -> Object

instance Shape Object where
  intersectWith r Ob{shape = s} = intersectWith r s