module Ray
  ( Ray (..),
  )
where

import Vector3

data Ray a = Ray
  { origin :: Vector3 a,
    direction :: Vector3 a
  }
  deriving (Show)
