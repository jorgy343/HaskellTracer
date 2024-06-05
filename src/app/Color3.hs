module Color3
  ( Color3 (..),
    Color (..),
    fromVector3,
  )
where

import Data.Word8
import Vector3

class Color color where
  saturate :: (Floating a, Ord a) => color a -> color a
  toWord8 :: (RealFrac a) => color a -> [Word8]

data Color3 a = Color3 a a a
  deriving (Show)

instance Functor Color3 where
  fmap f (Color3 r g b) = Color3 (f r) (f g) (f b)

instance Color Color3 where
  saturate = fmap $ min 1 . max 0
  toWord8 (Color3 r g b) = [toWord8' r, toWord8' g, toWord8' b, 0]
    where
      toWord8' x = round (x * 255) :: Word8

fromVector3 :: Vector3 a -> Color3 a
fromVector3 (Vector3 x y z) = Color3 x y z
