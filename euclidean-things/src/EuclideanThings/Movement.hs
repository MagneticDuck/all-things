-- module description -- {{{
{- |
  This module exports a datatype to represent an atomic movement,
  out of which 'Path's are composed; a movement is either a line
  segment or an arc, starting in the origin and ending in a
  determined point. 
-} -- }}}
module EuclideanThings.Movement
-- exports -- {{{
  ( 
  -- * Constructors
    PathMovement(..)
  , arcMovementFromVectorHeight
  , lineMovementFromVector 
  , arcMovementFromPivotDeltaAngle

  -- * Accessors
  , circleFromMovement 
  , centerFromMovement
  , anglesFromMovement
  , lengthFromMovement 
  , vectorFromMovement

  , positionFromMovementDistance 
  , cutFromMovementDistance

  -- * Intersection
  , intersectsOnMovement
  , reverseMovement

  , intersectPositionMovementLine
  , intersectPositionMovementRay
  , intersectPositionMovementCircle
  , intersectMovementPositionMovement
  , projectDistanceFromMovementPoint)
  where -- }}}

import EuclideanThings.Primitives
import Data.Function (on)
import Data.List
import Data.Maybe (maybeToList, fromJust)

-- * Constructors -- {{{
data PathMovement a = PathMovement (Vector a) a deriving (Show) 

-- | This is a synonym for the data constructor 'PathMovement';
-- given a vector and a relative arc height, it
-- returns a 'PathMovement'
arcMovementFromVectorHeight :: Vector a -> a -> PathMovement a 
arcMovementFromVectorHeight = PathMovement

-- | Given a vector, this constructs the shortest movement
-- connects the origin with the supplied vector
lineMovementFromVector :: (RealFloat a) => Vector a -> PathMovement a 
lineMovementFromVector = flip PathMovement 0 

pointOnArcSlice :: (RealFloat a) => Point a -> Angle a -> Angle a -> Point a -> Bool 
pointOnArcSlice cen a1 a2 p = -- {{{
  let
    pa = angleFromVector $ vectorFromTo cen p
    deltaA = angleFromTo a1 a2
    anglePred = flip (on (<) degreeFromAngle) deltaA
  in
    (&&)
      (anglePred (angleFromTo a1 pa))
      (anglePred (angleFromTo pa a2))   -- }}}
  
-- | This constructs a movement that, beginning in the origin,
-- rotates about a supplied pivot by the supplied delta angle
arcMovementFromPivotDeltaAngle :: (RealFloat a) => Point a -> Angle a -> PathMovement a 
arcMovementFromPivotDeltaAngle p da = -- {{{
  let
    circle =
      circleFromPointRadius p (lengthFromVector p)
    v = rotateVectorAbout p da nullVector
    midp = midpoint v nullVector
    ray = rayFromPointAngle p (addAngles (angleFromDegree (-90)) (angleFromVector v))
    apexi= intersectRayCircle ray circle
  in
    if da == angleFromDegree 0 then
      PathMovement nullVector 0
    else
      case apexi of
        [apex] ->
          let
            h = lengthFromVector (vectorFromTo midp apex) / lengthFromVector v 
          in
            PathMovement v h
        _ -> PathMovement v 0  -- }}}
 -- }}}
     
-- * Accessors -- {{{
-- | Returns the circle that the movement, 
-- executed from the origin, belongs to.
circleFromMovement :: (RealFloat a) => PathMovement a -> Maybe (Circle a) 
circleFromMovement (PathMovement v h) -- {{{
  | h == 0 = Nothing
  | otherwise =
      let
        apex = scaleVector 0.5 v + scaleVector h (rotateVector (angleFromDegree (-90)) v)
      in
        Just $ circleFromPointPointPoint nullVector apex v  -- }}}

-- | Returns the point that all points on the movement
-- are at an equal distance to. Does not exist if the
-- movement is linear.
centerFromMovement :: (RealFloat a) => PathMovement a -> Maybe (Point a) 
centerFromMovement = fmap centerFromCircle . circleFromMovement 

-- | Returns the clockwise interval angles that the 
-- supplied movement moves through on its circle.
anglesFromMovement :: (RealFloat a) => PathMovement a -> Maybe (Angle a, Angle a)  
anglesFromMovement m@(PathMovement v h) = 
  fmap
    (\cen -> 
      if h > 0 then
        ( angleFromVector (vectorFromTo cen nullVector)
        , angleFromVector (vectorFromTo cen v))
      else
        ( angleFromVector (vectorFromTo cen v)
        , angleFromVector (vectorFromTo cen nullVector) ) )
    (centerFromMovement m)

-- | Returns the length of the movement.
lengthFromMovement :: (RealFloat a) => PathMovement a -> a 
lengthFromMovement m = -- {{{
  case (circleFromMovement m, anglesFromMovement m) of
    (Just circle, Just (a1, a2)) ->
      let
        radius = radiusFromCircle circle
        deltaA = angleFromTo a1 a2
      in
        (2 * pi * radius) * (degreeFromAngle deltaA / 360)  
    _ -> lengthFromVector . vectorFromMovement $ m -- }}}
    
-- | Returns the point at which the movement, executed from
-- the origin, ends up at.
vectorFromMovement :: (RealFloat a) => PathMovement a -> Vector a 
vectorFromMovement (PathMovement v _) = v 

-- | Returns the point at 
positionFromMovementDistance :: (RealFloat a) => PathMovement a -> a -> Point a 
positionFromMovementDistance m@(PathMovement v 0) d = -- {{{
  let
    ml = lengthFromMovement m
  in
    if d >= 0 && d <= ml then
      vectorFromAngleLength (angleFromVector v) d
    else
      if d < 0 then nullVector else v -- }}}
positionFromMovementDistance m@(PathMovement v h) d = -- {{{
  let 
    ml = lengthFromMovement m
  in
    if (d >= 0) && (d <= ml) then
      case (circleFromMovement m, anglesFromMovement m) of
        (Just circle, Just (a1, a2)) -> 
          let
            radius = radiusFromCircle circle
            cen = centerFromCircle circle
            -- (a1, a2) always goes clockwise, but the movement does not always; hence the h<0,h>=0 discontinuitity
            ap = 
              addAngles a1 $
                if h < 0 then
                    applyAngle (((ml - d) / ml) *) $ angleFromTo a1 a2
                else
                    applyAngle ((d / ml) *) (angleFromTo a1 a2)
          in
            vectorFromAngleLength ap radius + cen
    else
      if d < 0 then nullVector else v  -- }}}

cutFromMovementDistance :: (RealFloat a) => PathMovement a -> a -> (PathMovement a, PathMovement a) 
cutFromMovementDistance m@(PathMovement v 0) d 
  | d >= 0 && d <= lengthFromMovement m =
      ( PathMovement (scaleVector d $ unitVector v) 0
      , (`PathMovement` 0)
          (scaleVector 
            (lengthFromMovement m - d) 
            (unitVector v))  )
  | otherwise = 
      if d < 0 then 
        (PathMovement nullVector 0, m) 
      else 
        (m, PathMovement nullVector 0) 
cutFromMovementDistance m d 
  | d >= 0 && d <= lengthFromMovement m = 
      case (centerFromMovement m, anglesFromMovement m) of
        (Just cen, Just (a1, a2)) -> 
          let
            ml = lengthFromMovement m
            da1 = applyAngle (* (d / ml)) (angleFromTo a1 a2)
            da2 = angleFromTo (addAngles a1 da1) a2
            v1 = rotateVectorAbout cen da1 nullVector
          in
            ( arcMovementFromPivotDeltaAngle cen da1 
            , arcMovementFromPivotDeltaAngle (cen - v1) da2)
  | otherwise =
      if d < 0 then (PathMovement nullVector 0, m) else (m, PathMovement nullVector 0)  
 -- }}}
 
-- * Intersection -- {{{
intersectsOnMovement :: (RealFloat a) => PathMovement a -> Point a -> Bool 
intersectsOnMovement (PathMovement v 0) p = 
  let
    ray1 = rayFromPointPoint nullVector v
    ray2 = rayFromPointPoint v nullVector
  in
    intersectsOnRay ray1 p && intersectsOnRay ray2 p
intersectsOnMovement m@(PathMovement v h) p =
  fromJust $
    (flip fmap $ circleFromMovement m)
      (\circle -> 
        let
          cen = centerFromCircle circle
          a1 = angleFromVector $ vectorFromTo cen nullVector
          a2 = angleFromVector $ vectorFromTo cen v
        in
          if h < 0 then
            pointOnArcSlice cen a2 a1 p
          else
            pointOnArcSlice cen a1 a2 p)

reverseMovement :: (RealFloat a) => PathMovement a -> PathMovement a 
reverseMovement (PathMovement v h) = PathMovement (negate v) (negate h) 
  
intersectPositionMovementLine 
  :: (RealFloat a) => Point a -> PathMovement a  -> Line a -> [Point a] 
intersectPositionMovementLine s m@(PathMovement v h) line1 = -- {{{
  let
    translateS = transformFromTranslate s
    untranslateS = transformFromTranslate (negate s)
  in
    case h of
      0 ->
        let
          line2 = 
            transformLine translateS $ 
              lineFromPointPoint nullVector v
          lineIntersect = maybeToList $ intersectLineLine line1 line2
        in
          filter (intersectsOnMovement m . transformPoint untranslateS) lineIntersect
      _ ->
        let
          circle1 = transformCircle translateS $ fromJust $ circleFromMovement m
          lineIntersect = intersectLineCircle line1 circle1
        in
          filter (intersectsOnMovement m . transformPoint untranslateS) lineIntersect  -- }}}
intersectPositionMovementRay 
  :: (RealFloat a) => Point a -> PathMovement a -> LineRay a -> [Point a] 
intersectPositionMovementRay s m@(PathMovement v h) ray1 = -- {{{
  let
    translateS = transformFromTranslate s
    untranslateS = transformFromTranslate (negate s)
  in
    case h of
      0 ->
        let
          line2 = 
            transformLine translateS $ 
              lineFromPointPoint nullVector v
          lineIntersect = maybeToList $ intersectLineRay line2 ray1
        in
          filter (intersectsOnMovement m . transformPoint untranslateS) lineIntersect
      _ ->
        let
          circle1 = transformCircle translateS $ fromJust $ circleFromMovement m
          lineIntersect = intersectRayCircle ray1 circle1
        in
          filter (intersectsOnMovement m . transformPoint untranslateS) lineIntersect  -- }}}

intersectPositionMovementCircle ::  
  (RealFloat a) => 
    Point a -> PathMovement a -> Circle a -> [Point a]
intersectPositionMovementCircle s m@(PathMovement v h) circle1 = -- {{{
  case h of
    0 ->
      let
        translateS = transformFromTranslate s
        untranslateS = transformFromTranslate (negate s)

        line2 = 
          transformLine translateS $ 
            lineFromPointPoint nullVector v
        lineIntersect = intersectLineCircle line2 circle1
      in
        filter (intersectsOnMovement m . transformPoint untranslateS) lineIntersect
    _ ->
      let
        translateS = transformFromTranslate s
        untranslateS = transformFromTranslate (negate s)

        circle2 = transformCircle translateS $ fromJust $ circleFromMovement m
        lineIntersect = intersectCircleCircle circle1 circle2
      in
        filter (intersectsOnMovement m . transformPoint untranslateS) lineIntersect  -- }}}
  
intersectMovementPositionMovement ::  
  (RealFloat a) => PathMovement a -> Point a -> PathMovement a -> [Point a]
intersectMovementPositionMovement  -- {{{
  m1@(PathMovement v1 0) p m2@(PathMovement v2 0) =
  let
    line1 = lineFromPointPoint nullVector v1
    line2 = lineFromPointPoint p (p + v2)
    intersects = maybeToList $ intersectLineLine line1 line2
    intersectsOnM1 = intersectsOnMovement m1 
    intersectsOnM2 px =
      intersectsOnMovement m2 (px - p)
  in
    filter (\px -> intersectsOnM1 px && intersectsOnM2 px) intersects -- }}}
intersectMovementPositionMovement m1 p m2@(PathMovement v2 0) = -- {{{
  let
    circle1 = fromJust $ circleFromMovement m1
    line1 = lineFromPointPoint p (p + v2)
    intersects = intersectCircleLine circle1 line1
    intersectsOnM1 = intersectsOnMovement m1 
    intersectsOnM2 px =
      intersectsOnMovement m2 (px - p)
  in
    filter (\px -> intersectsOnM1 px && intersectsOnM2 px) intersects  -- }}}
intersectMovementPositionMovement m1 p m2 = -- {{{
  let
    circle1 = fromJust $ circleFromMovement m1
    circle2 = transformCircle (transformFromTranslate p) $ fromJust $ circleFromMovement m2
    intersects = intersectCircleCircle circle1 circle2
    intersectsOnM1 = intersectsOnMovement m1 
    intersectsOnM2 px =
      intersectsOnMovement m2 (px - p)
  in
    filter (\px -> intersectsOnM1 px && intersectsOnM2 px) intersects  -- }}}

projectDistanceFromMovementPoint :: (RealFloat a) => PathMovement a -> Point a -> a 
projectDistanceFromMovementPoint m@(PathMovement v 0) p = -- {{{
  let
    line1 = lineFromPointPoint nullVector v
    projectLine = projectLinePoint line1 p
  in
    if intersectsOnMovement m projectLine then
      let
        pd = projectLine
      in
        lengthFromVector pd
    else
      let
        pd = minimumBy (compare `on` (lengthFromVector . vectorFromTo p)) [nullVector, v]
      in
        lengthFromVector pd -- }}}
projectDistanceFromMovementPoint m@(PathMovement v _) p = -- {{{
  let
    circle1 = fromJust $ circleFromMovement m
    cen = centerFromCircle circle1
    projectCircle = projectCirclePoint circle1 p
    (a1, a2) = fromJust $ anglesFromMovement m
  in
    if intersectsOnMovement m projectCircle then
      let
        da = angleFromTo a1 $ angleFromVector $ vectorFromTo cen projectCircle
        fulla = angleFromTo a1 a2
        part = degreeFromAngle da / degreeFromAngle fulla
      in
        lengthFromMovement m * part
    else
      let
        pd = minimumBy (compare `on` lengthFromVector . vectorFromTo p) [nullVector, v]
        pa = angleFromVector $ vectorFromTo cen pd
        da = angleFromTo a1 pa
        fulla = angleFromTo a1 a2
        part = degreeFromAngle da / degreeFromAngle fulla
      in
        lengthFromMovement m * part  -- }}}
 -- }}}
