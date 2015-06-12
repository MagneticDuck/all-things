-- module description -- {{{
{- |
  This module exports constructors and utilities with the objective
  of cleanly and simply handing angles and differences between
  them. An 'Angle' can function both as a direction and as a
  difference between two angles.
-} -- }}}
module EuclideanThings.Primitives.Angle
-- exports -- {{{
  ( 
  -- * Constructors
    Angle
  , angleFromDegree
  , angleFromRadian
  
  -- * Accessors
  , degreeFromAngle 
  , radFromAngle 
  
  -- * Transformations
  , applyAngle
  , invertAngle
  
  -- * Combinators
  , angleFromTo
  , addAngles
  ) where -- }}}

-- * Constructors  -- {{{
-- docs -- {{{
{- |
  The type of an angle; all math done on angles is kept
  in a bounded range representing a rotation of a circle
  in the expected manner.

  These bastards are suprisingly complex for something
  that schools teach so long before they start teaching
  calculus ;)
-} -- }}}
newtype Angle a = Angle { getAngle :: a } deriving (Eq, Ord)

cleanValue :: (RealFloat a) => a -> a 
cleanValue x 
  | any ($ x) [isNaN, isInfinite] = x -- {{{
  | otherwise =
      case signum x of
        -1 -> cleanValue (360 + x)
        _ -> 
          if x < 360 then x
          else cleanValue (x - 360)  -- }}}

-- | Creates an 'Angle' from a number. 0 degrees points straight up,
-- a circle has 360 degrees, counted *clockwise*.
angleFromDegree :: (RealFloat a) => a -> Angle a 
angleFromDegree a = Angle {getAngle = cleanValue a} 

-- | Same as angleFromDegree, but now 2*pi represents a full rotation.
angleFromRadian :: (RealFloat a) => a -> Angle a 
angleFromRadian = angleFromDegree . (* (360 / (2 * pi))) 
 -- }}}
 
-- * Accessors -- {{{
-- | Returns the degree value that an 'Angle' represents.
-- (degreeFromAngle . angleFromDegree) is only identity
-- for values in the interval [0, 360[. This function is guaranteed
-- to return values in the range [0, 360[
degreeFromAngle :: (RealFloat a) => Angle a -> a 
degreeFromAngle = getAngle 

-- | Same as 'degreeFromAngle', but returns the radian value,
-- guaranteed to be in the range [0, 2*pi[
radFromAngle :: (RealFloat a) => Angle a -> a 
radFromAngle = (* ((2 * pi) / 360)) . getAngle 
 -- }}}

-- * Transformations -- {{{
-- docs -- {{{
{- |
  This applies an arbitrary function to the contents of an angle.
  The transformation in question should operate on the value
  expecting it to be contained in the interval [0, 360[ 
  (an angle expressed in degrees).
-} -- }}}
applyAngle :: (RealFloat a) => (a -> a) -> Angle a -> Angle a 
applyAngle f = angleFromDegree . f . degreeFromAngle
--                      Degree   f   degree
--                   f is surrounded by degrees
-- I love my new <a>From<b> naming scheme 

-- docs -- {{{
{- |
  Equilivant to
  @
    applyAngle (+180)
  @
-} -- }}}
invertAngle :: (RealFloat a) => Angle a -> Angle a 
invertAngle = applyAngle (+ 180) 
 -- }}}
 
-- * Combinators -- {{{
-- docs -- {{{
{- |
  Given a1 and a2 this function returns a3 for which

  @ addAngles a1 a3 == a2 @

  (At least in theory; actual floating point values on actual
  computers don't always satisfy the laws of arithmetic.)
-} -- }}}
angleFromTo :: (RealFloat a) => Angle a -> Angle a -> Angle a 
angleFromTo a1 a2 = 
  angleFromDegree $
    if a2 < a1 then
      (360 - lita1) + lita2
    else
      lita2 - lita1
  where
    [lita1, lita2] = map degreeFromAngle [a1, a2]  

-- docs -- {{{
{- |
  For this to make any sense, one of the angles should be 
  considered a delta angle, not an actual direction in itself.

  @ addAngles $ angleFromDegree (-45) @ is a function that 
  shifts an angle counterclockwise by 45 degrees.

  This function is communitive.
-} -- }}}
addAngles :: (RealFloat a) => Angle a -> Angle a -> Angle a 
addAngles a1 a2 = applyAngle (+ degreeFromAngle a2) a1 
 -- }}}
 
instance (RealFloat a, Show a) => Show (Angle a) where 
  show = (++ "°") . show . degreeFromAngle 
