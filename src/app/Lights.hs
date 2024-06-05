module Lights
  ( Light (..),
  )
where

import Color3
import Vector3

data Light a = DirectionalLight
  { direction :: Vector3 a,
    intensity :: a,
    color :: Color3 a
  }
  deriving (Show)