-- module description -- {{{
{- |
  module : EuclideanThings.Primitives.LineAndCircle 

  This module exports constructors and accessors for lines, \"line
  rays\" (normally just called \"rays\"), and circles, along with 
  a large number of formulae for intersecting and projecting
  things onto other things.
-} -- }}}
module EuclideanThings.Primitives.LineAndCircle
-- exports -- {{{
  -- debugging, useful for PathMovement
  ( 
  -- * Miscellaneous
    intersectsOnRay
  , isOnRay

  -- * Lines and LineRays
  -- ** Constructors 
  , Line(..)
  , lineFromPointPoint
  , lineFromPointAngle
  , lineBetweenPointPoint
 
  , LineRay(..)
  , rayFromPointPoint
  , rayFromPointAngle

  -- ** Accessors 
  , angleFromLine
  , slopeFromLine
  , angleFromRay
  , slopeFromRay
  , startFromRay

  -- * Circles
  -- ** Constructors 
  , Circle(..)
  , circleFromPointRadius
  , circleFromPointPoint
  , circleFromPointPointPoint

  -- ** Accessors 
  , radiusFromCircle
  , centerFromCircle

  -- * Intersections
  , isOnLine -- dangerous.
  , isOnCircle -- also dangerous.
  , projectLinePoint  
  , projectCirclePoint

  , intersectLineLine
  , intersectLineRay
  , intersectLineCircle
  , intersectRayLine
  , intersectRayRay
  , intersectRayCircle
  , intersectCircleLine
  , intersectCircleRay
  , intersectCircleCircle ) where -- }}}

import EuclideanThings.Primitives.Vector
import EuclideanThings.Primitives.Angle
import Control.Monad (mfilter, join)

-- * Lines and LineRays
-- ** Constructors {{{

-- | This represents a line running through two 2d points,
-- continuing infinitely on each side
data Line a = Line (Point a) (Point a) deriving (Eq, Show)

data StandardLine a = StandardLine a a a

pointsToStandardLine  :: (RealFloat a) => Point a -> Point a -> StandardLine a  
pointsToStandardLine (Vector x1 y1) (Vector x2 y2)  
  | x1 == x2 = StandardLine 1 0 (-x1) 
  | y1 == y2 = StandardLine 0 1 (-y1) 
  | otherwise = 
      let m = (x1 - x2) / (y1 - y2) in 
        StandardLine 1 (-m) (y1 - m * x1)     

-- | This is simply a synonym of the data constructor 'Line'.
-- Given two points, it contructs a line that runs through them.
lineFromPointPoint :: (RealFloat a) => Point a -> Point a -> Line a 
lineFromPointPoint = Line 

-- | Given a point and an angle, represents a line that runs
-- through the point and has the direction of the supplied angle.
lineFromPointAngle :: (RealFloat a) => Point a -> Angle a -> Line a 
lineFromPointAngle a b = 
  lineFromPointPoint a (a + vectorFromAngle b) 

-- | Constructs the perpendicular bisector of the two points.
lineBetweenPointPoint :: (RealFloat a) => Point a -> Point a -> Line a 
lineBetweenPointPoint p1 p2 =
  lineFromPointAngle 
    (scaleVector (1/2) $ p1 + p2) 
    (addAngles (angleFromVector (p2 - p1)) (angleFromDegree 90)) 

-- | Represents a line ray, commonly known as a ray, that
-- \"begins\" on the first point and \"runs through\" the second.
data LineRay a = LineRay (Point a) (Point a)

-- | this is simply a synonym for the data constructor 'LineRay'
rayFromPointPoint :: (RealFloat a) => Point a -> Point a -> LineRay a 
rayFromPointPoint = LineRay 

-- | Represents a line ray that starts on a point and
-- has the direction of a supplied angle.
rayFromPointAngle :: (RealFloat a) => Point a -> Angle a -> LineRay a 
rayFromPointAngle p a = LineRay p (p + vectorFromAngle a) 
 -- }}}

-- ** Accessors -- {{{
-- | Returns (one of the possible) directions of the line in question in
-- the form of an 'Angle'
angleFromLine :: (RealFloat a) => Line a -> Angle a 
angleFromLine (Line p1 p2) = angleFromVector (p2 - p1) 

-- | Returns the slope of a line, Δy / Δx
slopeFromLine :: (RealFloat a) => Line a -> a 
slopeFromLine (Line p1 p2) =
  slopeFromVector $ p2 - p1

-- | Returns the direction of the ray in question in the form of an 'Angle'
angleFromRay :: (RealFloat a) => LineRay a -> Angle a 
angleFromRay (LineRay p1 p2) = 
  angleFromVector $ p2 - p1 

-- | Returns the slope of the ray, Δy / Δx
slopeFromRay :: (RealFloat a) => LineRay a -> a 
slopeFromRay (LineRay p1 p2) =
  slopeFromVector $ p2 - p1

-- | Returns the beginning of the ray
startFromRay :: (RealFloat a) => LineRay a -> Point a 
startFromRay (LineRay p1 _) = p1 
 -- }}}

-- * Circles 
-- ** Constructors -- {{{
data Circle a = Circle (Point a) a deriving (Show)

-- | From a point and a radius, constructs a circle 
circleFromPointRadius :: (RealFloat a) => Point a -> a -> Circle a 
circleFromPointRadius = Circle  

-- | Constructs the smallest circle to the supplied two points
circleFromPointPoint :: (RealFloat a) => Point a -> Point a -> Circle a 
circleFromPointPoint p1 p2 = 
  Circle (midpoint p1 p2) (lengthFromVector (p1 - p2) / 2) 

-- | Constructs the only circle to contain all three of the supplied points
circleFromPointPointPoint  
  :: (RealFloat a) => Point a -> Point a -> Point a -> Circle a  
circleFromPointPointPoint p1 p2 p3 = 
  let
    line1 = lineBetweenPointPoint p1 p2
    line2 = lineBetweenPointPoint p2 p3
    intersect = intersectLineLine line1 line2
  in
    case intersect of
      Just c -> circleFromPointRadius c (lengthFromVector $ c - p1)
      Nothing -> circleFromPointRadius nullVector 0    
 -- }}}
 
-- ** Accessors -- {{{
-- | Gets the radius of the supplied circle
radiusFromCircle :: (RealFloat a) => Circle a -> a 
radiusFromCircle (Circle _ rad) = rad 

-- | Gets the center of the supplied circle
centerFromCircle :: (RealFloat a) => Circle a -> Point a 
centerFromCircle (Circle c _) = c   
  -- }}}

-- * Intersections -- {{{
-- docs -- {{{
{- |
  Checks whether a point is contained within a line.
  If the numeric type you're using isn't completely perfect,
  this should not be used. Remember,
  @ 
  (0.01 :: Double) + 0.05 == 6.0000000000000005e-2 
  @
  
  Breaking news: computers work in base 2!
-} -- }}}
isOnLine :: (RealFloat a) => Line a -> Point a -> Bool 
isOnLine (Line va vb) (Vector x y) =
  let
    StandardLine a b c = pointsToStandardLine va vb
  in
    (a * x) + (b * y) + c == 0 

-- | Checks whether a point is contained on a circle.
-- Similar to 'isOnLine', this is dangerous if you are using
-- a non-perfect numeric type
isOnCircle :: (RealFloat a) => Circle a -> Point a -> Bool 
isOnCircle (Circle p r) p1 =
  lengthFromVector (p1 - p) == r  

-- | Projects a point on a line; finds the point on a line that 
-- minimizes distance to the supplied point.
projectLinePoint :: (RealFloat a) => Line a -> Point a -> Point a 
projectLinePoint (Line va@(Vector ax ay) vb@(Vector bx by)) (Vector cx cy) =  -- {{{
  let 
    l = lengthFromVector (va - vb)
    r = (((ay-cy)*(ay-by))-((ax-cx)*(bx-ax)))/(l*l)
  in Vector (ax + (r * (bx - ax))) (ay + (r * (by - ay)))    -- }}}

-- | Projects a point on a circle; finds the point on a circle
-- that minimizes distance to a supplied point.
projectCirclePoint :: (RealFloat a) => Circle a -> Point a -> Point a 
projectCirclePoint (Circle cen rad) p = 
  scaleVector rad (unitVector (p - cen)) + cen 

-- | Returns the intersection of two lines 
intersectLineLine :: (RealFloat a) => Line a -> Line a -> Maybe (Point a) 
intersectLineLine -- {{{
    (Line (Vector ax ay) (Vector bx by))
      (Line (Vector cx cy) (Vector dx dy)) =
  let
    rtop = ((ay-cy) * (dx-cx)) - ((ax-cx) * (dy-cy))
    rbottom = ((bx-ax) * (dy-cy)) - ((by-ay) * (dx-cx))
  in
    case rbottom of
      0 -> Nothing
      _ ->
        let r = rtop / rbottom in
          Just $ Vector (ax + (r * (bx-ax))) (ay+(r * (by-ay)))   -- }}}

isOnRay :: (RealFloat a) => LineRay a -> Point a -> Bool 
isOnRay = intersectsOnRay 

intersectsOnRay :: RealFloat a => LineRay a -> Point a -> Bool 
intersectsOnRay (LineRay p1 p2) p = -- {{{
  let
    [pa, p2a] = 
      map (angleFromVector . vectorFromTo p1) [p, p2]
  in
    any (<= angleFromDegree 90)
      [ angleFromTo p2a pa
      , angleFromTo pa p2a ]  -- }}}

-- | Finds the intersection of a line with a ray; guaranteed to
-- return one point or nothing
intersectLineRay :: (RealFloat a) => Line a -> LineRay a -> Maybe (Point a) 
intersectLineRay l r@(LineRay p21 p22) =
  mfilter (intersectsOnRay r) $ intersectLineLine l (Line p21 p22) 
  
intersectLineBaseCircle :: (RealFloat a) => Line a -> a -> [Point a] 
intersectLineBaseCircle (Line (Vector dx1 dy1) (Vector dx2 dy2)) r = -- {{{
  let
    y1 = maximum [dy1, dy2]
    y2 = minimum [dy1, dy2]
    x1 = if dy1 == y1 then dx1 else dx2
    x2 = if dy1 == y1 then dx2 else dx1
    dx = x2 - x1
    dy = y2 - y1
    dr = lengthFromVector (Vector dx dy)
    d = (x1 * y2) - (x2 * y1)
    sq = join (*)
    sgn x =
      case signum x of
        (-1) -> 1
        _ -> 1
    disc = sq (r * dr) - sq d
  in
  case signum disc of
    (-1) -> []
    _ ->
      if dr == 0 then
        []
      else
        (\[x, y] -> if x == y then [x] else [x, y])
          [
            Vector
              ((d * dy - (sgn dy * dx * sqrt disc)) / (dr * dr))
              ((((-d) * dx) + (abs dy * sqrt disc)) / (dr * dr))
          ,
            Vector
              (((d * dy) + (sgn dy * dx * sqrt disc)) / (dr * dr))
              ((((-d) * dx) - (abs dy * sqrt disc)) / (dr * dr))
          ]

-- I just intersected a line and a circle in 32 lines of code.
-- That was sad.
-- But I'm not going back.
-- Never.

-- Edit: MagneticDuck from the future
  -- look at me! I'm back in this terrible library!
  -- but I'm still not going to refactor this shit.  -- }}}

-- | Intersects a line with a circle; guaranteed to return a list with 
-- a length between 0 and 2 elements long.
intersectLineCircle :: (RealFloat a) => Line a -> Circle a -> [Point a] 
intersectLineCircle (Line p1 p2) (Circle center rad) =
  map (+ center)
    (intersectLineBaseCircle (Line (p1 - center) (p2 - center)) rad) 

-- | Intersects a ray with a line; guaranteed to either return
-- one point or nothing
intersectRayLine :: (RealFloat a) => LineRay a -> Line a -> Maybe (Point a) 
intersectRayLine = flip intersectLineRay 

-- | Intersects two rays
intersectRayRay :: (RealFloat a) => LineRay a -> LineRay a -> Maybe (Point a)  
intersectRayRay r1 r2 =
  mfilter (\p -> intersectsOnRay r1 p && intersectsOnRay r2 p) $
    intersectLineLine (rayToLine r1) (rayToLine r2) 
  where
    rayToLine (LineRay p1 p2) = Line p1 p2  

-- | Intersects a ray with a circle; guaranteed to return either
-- a list with a length from 0 to 2 element long.
intersectRayCircle :: (RealFloat a) => LineRay a -> Circle a -> [Point a] 
intersectRayCircle r@(LineRay p1 p2) c =
  filter (intersectsOnRay r) $
    intersectLineCircle (Line p1 p2) c 

-- | Intersects a circle with a line; guaranteed to return a
-- list with a length from 0 to 2 elements long.
intersectCircleLine :: (RealFloat a) => Circle a -> Line a -> [Point a] 
intersectCircleLine = flip intersectLineCircle 

-- | Intersects a circle with a ray; guaranteed to return a list
-- with a length from 0 to 2 elements long.
intersectCircleRay :: (RealFloat a) => Circle a -> LineRay a -> [Point a] 
intersectCircleRay = flip intersectRayCircle 

-- | Intersects two circles; guaranteed to return a list with a
-- length from 0 to 2 elements long.
intersectCircleCircle :: (RealFloat a) => Circle a -> Circle a -> [Point a]  
intersectCircleCircle (Circle ip1 ir1) (Circle ip2 ir2) = 
  let -- {{{
    p1 = if ir1 >= ir2 then ip1 else ip2
    p2 = if ir1 >= ir2 then ip2 else ip1
    r1 = if ir1 >= ir2 then ir1 else ir2
    r2 = if ir1 >= ir2 then ir2 else ir1
    d = lengthFromVector (p1 - p2)
    [r12, r22, d2] = map (join (*)) [r1, r2, d]
    h = sqrt (r22 - join (*) (r22 + d2 - r12) / (2 * d))
    a = sqrt (r12 - join (*) h)
    v1 = unitVector (p2 - p1)
  in
    case signum $ d - (r1 + r2) of
      (-1) -> 
        [
          (+)
            (scaleVector a v1 + p1)
            (scaleVector h (rotateVector (angleFromDegree 90) v1))
        ,
          (+)
            (scaleVector a v1 + p1)
            (scaleVector h (rotateVector (angleFromDegree (-90)) v1)) ]
      0 -> 
        [scaleVector a (unitVector (p2 - p1)) + p1]
      1 -> []  -- }}} -- }}}
