{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Codec.BMP
import Color3
import Data.ByteString
import Lights
import Ray
import Shape
import Vector3

imageWidth :: Int
imageWidth = 640

imageHeight :: Int
imageHeight = 480

aspectRatio :: (Fractional a) => a
aspectRatio = fromIntegral imageWidth / fromIntegral imageHeight

scene :: Shape Double
scene = Sphere (Vector3 0 0 4) 1

light :: (Floating a) => Light a
light = DirectionalLight (Vector3 0 (-1) 0) 10 (Color3 1 1 1)

main :: IO ()
main = do
  let rgb =
        Data.ByteString.pack
          [ color
            | j <- [0 .. imageHeight - 1],
              i <- [0 .. imageWidth - 1],
              let ray = createProjectionRay (Vector3 0 1 0) (Vector3 0 0 4) (Vector3 0 1 0) (1.5708 :: Double) (1.0 :: Double) (fromIntegral i) (fromIntegral j),
              color <- toWord8 $ saturate $ traceScene ray scene
          ]
  let bmp = packRGBA32ToBMP24 imageWidth imageHeight rgb
  writeBMP "test.bmp" bmp

missShader :: (Floating a) => Ray a -> Color3 a
missShader _ = Color3 0 0 0

hitShaderBasic :: (Floating a, Ord a) => Intersection a -> Light a -> Color3 a
hitShaderBasic intersection (DirectionalLight lightDirection lightIntensity lightColor)
  | lightAngle > 0 = fromVector3 $ Vector3 0 0 1 .* lightAngle .* lightIntensity
  | otherwise = Color3 0 0 0
  where
    lightAngle = Vector3.negate lightDirection `dot` hitNormal intersection

traceScene :: (Floating a, Ord a) => Ray a -> Shape a -> Color3 a
traceScene ray scene =
  case intersectRay ray scene of
    Just intersection@(Intersection _ _ hitNormal) -> hitShaderBasic intersection light
    Nothing -> missShader ray

createProjectionRay :: (Floating a) => Vector3 a -> Vector3 a -> Vector3 a -> a -> a -> Int -> Int -> Ray a
createProjectionRay origin lookAt up fov near i j =
  Ray origin direction
  where
    n = normalize (lookAt .-. origin)
    u = normalize (up `cross` n)
    v = Vector3.negate (n `cross` u)
    h = tan (fov / 2.0) * 2.0 * near
    w = h * aspectRatio
    c = origin .+. n .* near
    l = c .-. u .* (w / 2.0) .-. v .* (h / 2.0)
    du = w / fromIntegral imageWidth
    dv = h / fromIntegral imageHeight
    x = u .* fromIntegral i .* du
    y = v .* fromIntegral j .* dv
    direction = normalize (l .+. x .+. y .-. origin)