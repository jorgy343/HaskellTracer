module Color3
  ( Color3 (..),
    toWord8,
    fromVector3,
  )
where

import Data.Word8
import Vector3

data Color3 a = Color3 a a a
  deriving (Show)

toWord8 :: (RealFrac a) => Color3 a -> [Word8]
toWord8 (Color3 r g b) = [toWord8' r, toWord8' g, toWord8' b, 0]
  where
    toWord8' x = round (x * 255) :: Word8

fromVector3 :: Vector3 a -> Color3 a
fromVector3 (Vector3 x y z) = Color3 x y z
