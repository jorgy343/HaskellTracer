module Shape (Shape (..), Intersection (..), intersectRay) where

import Data.Maybe (mapMaybe)
import Ray
import Vector3

data Shape a
  = ShapeList [Shape a]
  | Sphere
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

instance (Eq a) => Eq (Intersection a) where
  (Intersection hitDistance1 _ _) == (Intersection hitDistance2 _ _) = hitDistance1 == hitDistance2

instance (Ord a) => Ord (Intersection a) where
  compare (Intersection hitDistance1 _ _) (Intersection hitDistance2 _ _) = compare hitDistance1 hitDistance2

intersectRay :: (Floating a, Ord a) => Ray a -> Shape a -> Maybe (Intersection a)
intersectRay ray (ShapeList shapes)
  | null intersections = Nothing
  | otherwise = Just $ minimum intersections
  where
    intersections = mapMaybe (intersectRay ray) shapes
intersectRay (Ray rayOrigin rayDirection) (Sphere center radius)
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
intersectRay (Ray rayOrigin rayDirection) (Plane normal distance)
  | denom <= 0.0 = Nothing
  | otherwise =
      let hitDistance = (distance - (normal `dot` rayOrigin)) / denom
          hitPoint = rayOrigin .+. rayDirection .* hitDistance
       in Just (Intersection hitDistance hitPoint normal)
  where
    denom = normal `dot` rayDirection