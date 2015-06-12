-- module description -- {{{
{- |
  This module provides a simple definition of a 2-dimensional
  vector, and a host of methods for making dealing with them
  quite easy. This is probably not fast, but it also does not
  need to be.
-} -- }}}
module EuclideanThings.Primitives.Vector 
-- exports -- {{{
  (
  -- * Constructors 
  Vector (..)
  , Point
  , nullVector
  , vectorFromTo
  , vectorFromAngle
  , vectorFromAngleLength

  -- * Accessors
  , xFromVector
  , yFromVector
  , lengthFromVector
  , angleFromVector
  , angleFromPoints
  , slopeFromVector

  -- * Transformations
  , unitVector
  , scaleVector
  , rotateVector
  , rotateVectorAbout
  , midpoint
  , liftVector
  ) where
-- }}}

import EuclideanThings.Primitives.Angle
import Control.Monad (join)

-- * Constructors -- {{{
-- | Represents a vector in 2 dimensions, using the supplied type
-- parameter as (numeric) data type to hold the value of 
-- each dimension
data Vector a = Vector a a deriving (Show, Eq) 

-- | Represents a point in 2 dimensions using a the supplied type
-- parameter as (numeric) data type to hold the value of each
-- dimension; a type synonym for Vector
type Point a = Vector a 

-- | A null vector
nullVector :: (RealFloat a) => Vector a 
nullVector = Vector 0 0 

-- | Constructs a vector from one point to another; the vector
-- assumes the value of their difference
vectorFromTo :: (RealFloat a) => Point a -> Point a -> Vector a 
vectorFromTo p1 p2 = p2 - p1 

-- | Constructs a unit vector with the supplied angle as direction
vectorFromAngle :: (RealFloat a) => Angle a -> Vector a 
vectorFromAngle angle = 
  Vector (cos a) (sin a)
    where a = (radFromAngle angle * (-1)) + (pi / 2) 

-- | Constructs a vector with the supplied angle as direction
-- and norm equal to supplied value
vectorFromAngleLength :: (RealFloat a) => Angle a -> a -> Vector a 
vectorFromAngleLength a len =
  scaleVector len $ vectorFromAngle a 
 -- }}}

-- * Accessors -- {{{
-- | Accesses the x coordinate of the supplied vector
xFromVector :: Vector a -> a   
xFromVector (Vector x _) = x  

-- | Accesses the y coordinate of the supplied vector
yFromVector :: Vector a -> a  
yFromVector (Vector _ y) = y  

-- | Calculates the norm or length of the supplied vector
lengthFromVector :: (RealFloat a) => Vector a -> a 
lengthFromVector (Vector x y) = 
  sqrt $ (x ^ v) + (y ^ v)
  where v = 2 :: Int 

-- | Calculates the angle of the supplied vector
angleFromVector :: (RealFloat a) => Vector a -> Angle a 
angleFromVector (Vector x y) =
  let 
    a = atan2 x y
  in
    angleFromRadian a 

-- | Given a 3-tuple with points (p1, p2, p3), calculates the angle
-- traditionally stated as ∠p1p2p3
angleFromPoints :: (RealFloat a) => (Point a, Point a, Point a) -> Angle a
angleFromPoints (p1, p2, p3) =
  angleFromTo (angleFromVector (p1 - p2)) (angleFromVector (p3 - p2))

-- | Calculates the slope (Δy / Δx) of the supplied vector
slopeFromVector :: (RealFloat a) => Vector a -> a 
slopeFromVector (Vector x y) = y / x 

 -- }}}

-- * Transformations -- {{{
-- | Returns a vector collinear with the supplied vector but
-- with norm of 1, unless the supplied vector is null in which
-- case it acts as identity.
unitVector :: (RealFloat a) => Vector a -> Vector a 
unitVector (Vector 0 0) = nullVector
unitVector x = scaleVector (recip $ lengthFromVector x) x 

-- | Multiplies each coordinate of a vector by a scalar
scaleVector :: (RealFloat a) => a -> Vector a -> Vector a 
scaleVector scalar (Vector x y) = Vector (scalar * x) (scalar * y) 

-- | Rotates a vector around the origin by the supplied angle.
-- 
-- @ rotateVector (angleFromDegree 90) @ rotates a vector by 90 
-- degrees in a clockwise direction
rotateVector :: (RealFloat a) => Angle a -> Vector a -> Vector a 
rotateVector a v = 
  scaleVector (lengthFromVector v) $ vectorFromAngle (addAngles (angleFromVector v) a) 

-- | Same as 'rotateVector', but with a non-null center of rotation.
rotateVectorAbout ::  (RealFloat a) => Point a -> Angle a -> Vector a -> Vector a 
rotateVectorAbout p a v =
  p + rotateVector a (v - p) 

-- | Computes the midpoint of two points
midpoint :: (RealFloat a) => Point a -> Point a -> Point a 
midpoint v1 v2 = scaleVector (1/2) (v1 + v2) 

-- | Applies a supplied function to both coordinates of a vector.
-- @ liftVector realToFrac @ could be a potential use of this
-- function.
liftVector :: (a -> b) -> Vector a -> Vector b 
liftVector f (Vector x y) = Vector (f x) (f y)  -- }}}

instance (RealFloat a) => Num (Vector a) where 
  (Vector x1 y1) + (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
  negate = liftVector negate 
  abs = liftVector abs 
  (Vector x1 y1) * (Vector x2 y2) = Vector (x1 * x2) (y1 * y2)
  fromInteger = join Vector . fromInteger 
  signum = unitVector 
