module Main where

import Codec.BMP
import Color3
import Data.ByteString
import Ray
import Shape
import Vector3

imageWidth :: Int
imageWidth = 640

imageHeight :: Int
imageHeight = 480

aspectRatio :: (Fractional a) => a
aspectRatio = fromIntegral imageWidth / fromIntegral imageHeight

main :: IO ()
main = do
  let rgb =
        Data.ByteString.pack
          [ color
            | j <- [0 .. imageHeight - 1],
              i <- [0 .. imageWidth - 1],
              let ray = createProjectionRay (Vector3 0 0 0) (Vector3 0 0 1) (Vector3 0 1 0) (1.0 :: Double) (fromIntegral i) (fromIntegral j),
              color <- toWord8 $ fromVector3 $ direction ray
          ]
  let bmp = packRGBA32ToBMP24 imageWidth imageHeight rgb
  writeBMP "test.bmp" bmp

createProjectionRay :: (Floating a) => Vector3 a -> Vector3 a -> Vector3 a -> a -> Int -> Int -> Ray a
createProjectionRay origin lookAt up near i j =
  Ray origin (l .+. x .+. y)
  where
    n = normalize (lookAt .-. origin)
    u = normalize (up `cross` n)
    v = n `cross` u
    h = tan (aspectRatio / 2.0) * 2.0 * near
    w = h * aspectRatio
    c = origin .+. n .* near
    l = c .-. u .* (w / 2.0) .-. v .* (h / 2.0)
    du = w / fromIntegral imageWidth
    dv = h / fromIntegral imageHeight
    x = u .* fromIntegral i .* du
    y = v .* fromIntegral j .* dv

createProjectionRays :: (Floating a) => Vector3 a -> Vector3 a -> Vector3 a -> a -> [Ray a]
createProjectionRays origin lookAt up near =
  [ Ray origin (l .+. x .+. y)
    | j <- [0 .. imageHeight - 1],
      i <- [0 .. imageWidth - 1],
      let x = u .* fromIntegral i .* du,
      let y = v .* fromIntegral j .* dv
  ]
  where
    n = normalize (lookAt .-. origin)
    u = normalize (up `cross` n)
    v = n `cross` u
    h = tan (aspectRatio / 2.0) * 2.0 * near
    w = h * aspectRatio
    c = origin .+. n .* near
    l = c .-. u .* (w / 2.0) .-. v .* (h / 2.0)
    du = w / fromIntegral imageWidth
    dv = h / fromIntegral imageHeight
