module Vector3
  ( Vector3 (..),
    Vector (..),
    cross,
  )
where

class Vector vector where
  (.+.) :: (Floating a) => vector a -> vector a -> vector a
  infixl 6 .+.
  (.+) :: (Floating a) => vector a -> a -> vector a
  infixl 6 .+
  (+.) :: (Floating a) => a -> vector a -> vector a
  infixl 6 +.
  (.-.) :: (Floating a) => vector a -> vector a -> vector a
  infixl 6 .-.
  (.-) :: (Floating a) => vector a -> a -> vector a
  infixl 6 .-
  (-.) :: (Floating a) => a -> vector a -> vector a
  infixl 6 -.
  (.*) :: (Floating a) => vector a -> a -> vector a
  infixl 7 .*
  (*.) :: (Floating a) => a -> vector a -> vector a
  infixl 7 *.
  (./) :: (Floating a) => vector a -> a -> vector a
  infixl 7 ./
  dot :: (Floating a) => vector a -> vector a -> a
  infixl 7 `dot`
  negate :: (Floating a) => vector a -> vector a
  normalize :: (Floating a) => vector a -> vector a

data Vector3 a = Vector3 a a a
  deriving (Show)

instance Functor Vector3 where
  fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

instance Vector Vector3 where
  (.+.) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
  (.+) (Vector3 x y z) s = Vector3 (x + s) (y + s) (z + s)
  (+.) s (Vector3 x y z) = Vector3 (s + x) (s + y) (s + z)
  (.-.) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)
  (.-) (Vector3 x y z) s = Vector3 (x - s) (y - s) (z - s)
  (-.) s (Vector3 x y z) = Vector3 (s - x) (s - y) (s - z)
  (.*) (Vector3 x y z) s = Vector3 (x * s) (y * s) (z * s)
  (*.) s (Vector3 x y z) = Vector3 (s * x) (s * y) (s * z)
  (./) (Vector3 x y z) s = Vector3 (x / s) (y / s) (z / s)
  dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
  negate (Vector3 x y z) = Vector3 (-x) (-y) (-z)
  normalize (Vector3 x y z) = Vector3 (x / l) (y / l) (z / l)
    where
      l = sqrt (x * x + y * y + z * z)

infixl 7 `cross`

cross :: (Floating a) => Vector3 a -> Vector3 a -> Vector3 a
cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
  Vector3
    (y1 * z2 - z1 * y2)
    (z1 * x2 - x1 * z2)
    (x1 * y2 - y1 * x2)