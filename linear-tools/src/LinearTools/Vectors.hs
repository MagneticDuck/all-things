module LinearTools.Vectors
  (Vector (..)
  , scaleVector
  , Point
  , getX
  , getY
  , vectorAngle
  , vectorLength) where

data Vector = Vector Float Float deriving (Eq, Show)
type Point = Vector
 
instance Num Vector where
   (Vector x1 y1) + (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
   negate (Vector x y) = Vector (-x) (-y)
   abs (Vector x y) = Vector (abs x) (abs y)
   (Vector x1 y1) * (Vector x2 y2) = undefined
   fromInteger int = Vector (fromInteger int) (fromInteger int)
   signum = unitVect

vectorToAngle :: Vector -> Float
vectorToAngle vect =
  let
    x = getX vect
    y = getY vect
  in
    case x of
      0.0 -> pi/2.0
      _ -> 
        atan (y/x)

angleToVector :: Float -> Vector
angleToVector angle =
  Vector (cos angle) $ (sin angle)

unitVect :: Vector -> Vector
unitVect vect = scaleVector (recip $ vectorLength vect) vect
 
scaleVector :: Float -> Vector -> Vector
scaleVector scalar (Vector x y) =
  Vector (x * scalar) (y * scalar)
 
getX (Vector x _) = x
getY (Vector _ y) = y

vectorLength :: Vector -> Float
vectorLength (Vector x y) = sqrt $ (x^2) + (y^2)

vectorAngle :: Vector -> Float
vectorAngle (Vector x y) = (pi/2)-(atan2 x y)
