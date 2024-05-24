module Shape
  ( Sphere (..),
  )
where

import Vector3

data Sphere a = Sphere
  { center :: Vector3 a,
    radius :: a
  }
  deriving (Show)