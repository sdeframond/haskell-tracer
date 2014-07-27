{-# LANGUAGE GADTs #-}

module Tracer.Shapes ( colorFromMaterial
                     , Material(..)
                     , Color(..)
                     , Ray(..)
                     , Shape(..)
                     , Triangle(..)
                     , Sphere(..)
                     , Object(..)
                     , Light(..)
                     , Plane(..)
                   ) where

import           Data.Vect

type Point = Vec3
type Direction = Vec3

data Color = Color Float Float Float

instance Pointwise Color where
  pointwise (Color r g b) (Color r' g' b') = Color (r*r') (g*g') (b*b')

instance AbelianGroup Color where
  (&+) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)
  (&-) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1-r2) (g1-g2) (b1-b2)
  neg  (Color r g b)                     = Color (-r) (-g) (-b)
  zero = Color 0 0 0

instance Vector Color where
  scalarMul s (Color r g b) = Color (s*r) (s*g) (s*b)
  mapVec    f (Color r g b) = Color (f r) (f g) (f b)

data Material = Material { mShininess :: Float
                         , mSpec :: Color
                         , mDiff:: Color
                         }

colorFromMaterial :: Material -> Direction -> Point -> Direction -> Light -> Color
colorFromMaterial m v p n l = d &+ s &+ a
  where d = (mDiff m) &! (diffuseAt p n l)
        s = (mSpec m) &! (specularAt p n l v $ mShininess m)
        a = (mDiff m) &! (ambiantAt p l)

specularAt :: Vec3 -> Vec3 -> Light -> Vec3 -> Float -> Color
specularAt p n (Light pl c) v s = c &* factor
  where factor = if f > 0 then f ** s else 0
        f = r &. v
        r = (2 * (vecToLight &. n)) *& n &- vecToLight
        vecToLight = normalize $ p &- pl

diffuseAt :: Vec3 -> Vec3 -> Light -> Color
diffuseAt p n (Light pl c) = c &* factor
  where factor =  max 0 $ (n &. normalize vecToLight) / dist**2
        dist = norm vecToLight
        vecToLight = pl &- p

ambiantAt :: Vec3 -> Light -> Color
ambiantAt p (Light pl c) = c &* (1/(norm $ pl &- p)**2)

data Ray = Ray Point Direction

data Light = Light Point Color

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

data Object where
  Ob :: Shape a => { shape :: a
                   , material :: Material
                   } -> Object

instance Shape Object where
  intersectWith r Ob{shape = s} = intersectWith r s
  normal Ob{shape = s} p = normal s p