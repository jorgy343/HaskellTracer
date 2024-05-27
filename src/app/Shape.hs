module Shape
  ( Shape (..),
    Intersection (..),
    intersectRay,
  )
where

import Ray
import Vector3

data Shape a
  = Sphere
      { center :: Vector3 a,
        radius :: a
      }
  | Plane
      { normal :: Vector3 a,
        distance :: a
      }
  deriving (Show)

data Intersection a = Intersection
  { hitDistance :: a,
    hitPoint :: Vector3 a,
    hitNormal :: Vector3 a
  }
  deriving (Show)

intersectRay :: (Floating a, Ord a) => Shape a -> Ray a -> Maybe (Intersection a)
intersectRay (Sphere center radius) (Ray rayOrigin rayDirection)
  | discriminant <= 0.0 = Nothing
  | otherwise =
      let hitDistance = (-b - sqrt discriminant) / (2.0 * a)
          hitPoint = rayOrigin .+. rayDirection .* hitDistance
          hitNormal = normalize (hitPoint .-. center)
       in Just (Intersection hitDistance hitPoint hitNormal)
  where
    oc = rayOrigin .+. center
    a = rayDirection `dot` rayDirection
    b = 2.0 * (oc `dot` rayDirection)
    c = (oc `dot` oc) - radius * radius
    discriminant = b * b - 4.0 * a * c
intersectRay (Plane normal distance) (Ray rayOrigin rayDirection) = Nothing