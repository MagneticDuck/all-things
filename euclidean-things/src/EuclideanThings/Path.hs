-- This module exports a definition of a path through 2d
-- Euclidean space, and a host of constructors, 
module EuclideanThings.Path
  -- exports -- {{{
  ( 
  -- * Paths
  -- ** Constructors 
  Path(..)
  , pathFromPoints
  , pathFromPolygon
  , linePathFromPointPoint
  , linePathFromVector
  , arcPathFromCircleAngleAngle
  , arcPathFromPointPointHeight
  , arcPathFromVectorHeight

    -- ** Combinators
  , concatPaths

    -- ** Transformations
  , transformPath
  , putPathOn
  , appendPointToPath
  , reversePath

  -- ** Accessors
  , pathStartPosition
  , pathEndPosition

  -- * PathLocations 
  -- ** Constructors
  , PathLocation(..)
  , locationFromDistance  
  , pathStartLocation
  , pathEndLocation

  -- ** Accessors
  , positionFromLocation
  , distanceFromLocation

  -- ** Cutting Paths
  , cutPathAt

  -- * PositionMovement
  -- ** Constructors
  , PositionMovement(..)
  , pathToPositionMovements
  
  -- * Path Intersection 
  , intersectPathPath
  , intersectPathLine
  , intersectPathRay
  , intersectPathCircle
  , projectPathPosition
  , projectPathLocation
  ) where -- }}}

-- imports -- {{{
import Data.List
import Data.Function (on)

import EuclideanThings.Movement
import EuclideanThings.Primitives -- }}}

-- * Path
-- ** Constructors -- {{{
data Path a = Path (Point a) [PathMovement a] deriving (Show)

connectingVectors :: (RealFloat a) => [Point a] -> [Vector a] 
connectingVectors (p1:p2:ps) = (p2 - p1) : (connectingVectors (p2:ps)) 
connectingVectors _ = [] 

pathFromPoints :: (RealFloat a) => [Point a] -> Path a                    
pathFromPoints [] = Path nullVector []
pathFromPoints ps = 
  Path (head ps) $  map (flip PathMovement 0) (connectingVectors ps) 

pathFromPolygon :: (RealFloat a) => Integer -> Path a 
pathFromPolygon i =
  let
    angles = map (angleFromDegree . (* 360) . (/(fromIntegral i)) . fromIntegral) $ ([0 .. i] :: [Integer])
  in
    pathFromPoints $ (map vectorFromAngle angles)  

linePathFromPointPoint :: (RealFloat a) => Point a -> Point a -> Path a 
linePathFromPointPoint p1 p2 = 
  Path p1 [ PathMovement (p2 - p1) 0 ] 

linePathFromVector :: (RealFloat a) => Vector a -> Path a 
linePathFromVector v =
  linePathFromPointPoint nullVector v 

arcPathFromCircleAngleAngle 
  :: (RealFloat a) => Circle a -> Angle a -> Angle a -> Path a 
arcPathFromCircleAngleAngle circle a1 a2 = -- {{{
  let
    cen = centerFromCircle circle
    rad = radiusFromCircle circle
    da = angleFromTo a1 a2
    sp = (vectorFromAngleLength a1 rad) + cen
  in
    if da > (angleFromDegree 200) then
      concatPaths
        [ arcPathFromCircleAngleAngle circle a1 (addAngles a1 (angleFromDegree 180))
        , arcPathFromCircleAngleAngle circle (addAngles a1 (angleFromDegree 180)) a2 ]
    else
      Path sp 
        [ arcMovementFromPivotDeltaAngle (vectorFromTo sp cen) da ]  -- }}}

arcPathFromPointPointHeight :: (RealFloat a) => Point a -> Point a -> a -> Path a 
arcPathFromPointPointHeight p1 p2 h =
  Path p1 [ PathMovement (p2 - p1) h ] 

arcPathFromVectorHeight :: (RealFloat a) => Vector a -> a -> Path a 
arcPathFromVectorHeight v h =
  arcPathFromPointPointHeight nullVector v h 

appendPathToPath :: (RealFloat a) => Path a -> Path a -> Path a 
appendPathToPath (Path s1 ms1) (Path _ ms2)=
  Path s1 (ms1 ++ ms2) 
 -- }}}
 
-- ** Combinators -- {{{
concatPaths :: (RealFloat a) => [Path a] -> Path a 
concatPaths [] = Path nullVector []
concatPaths ps =
  putPathOn (pathStartPosition . head $ ps) $ 
    foldl appendPathToPath (linePathFromVector nullVector) ps 
 -- }}}
 
-- ** Transformations -- {{{
transformPath :: (RealFloat a) => Transform a -> Path a -> Path a 
transformPath t (Path s []) = Path (transformPoint t s) []
transformPath t (Path s ((PathMovement v h):ms)) = -- {{{
  appendPathToPath
    (arcPathFromPointPointHeight np1 np2 nh)
    (transformPath t $ (Path (s + v) ms))
  where 
    nh =
      if transformIsEven t then
        h
      else
        negate h
    [p1, p2] = [s, (s + v)]
    [np1, np2] = map f [p1, p2]
    f = transformPoint t  -- }}}

putPathOn :: (RealFloat a) => Point a -> Path a -> Path a 
putPathOn  p1 (Path _ ms) = Path p1 ms 

appendPointToPath :: (RealFloat a) => Point a -> Path a -> Path a 
appendPointToPath v p =
  concatPaths
    [ p , linePathFromVector (vectorFromTo (pathEndPosition p) v) ] 

reversePath :: (RealFloat a) => Path a -> Path a 
reversePath p@(Path _ ms) =
  Path 
    (pathEndPosition p)
    (reverse . (map reverseMovement) $ ms) 
 -- }}}   

-- ** Accessors -- {{{
pathStartPosition :: (RealFloat a) => Path a -> Point a 
pathStartPosition (Path s _) = s 

pathEndPosition :: (RealFloat a) => Path a -> Point a 
pathEndPosition (Path s []) = s
pathEndPosition (Path s ms) = 
  let
    lastMove = last ms
    initPath = (Path s (init ms))
  in
    case lastMove of
      PathMovement v _ -> v + (pathEndPosition initPath) 
 -- }}}

-- * PathLocation
-- ** Constructors -- {{{
newtype PathLocation a = PathLocation { getDistance :: a } deriving (Show)

locationFromDistance :: (RealFloat a) => a -> PathLocation a 
locationFromDistance d = PathLocation { getDistance = d } 
    
pathStartLocation :: (RealFloat a) => Path a -> Point a 
pathStartLocation p = 
  positionFromLocation p (locationFromDistance 0) 

pathEndLocation :: (RealFloat a) => Path a -> PathLocation a 
pathEndLocation (Path _ ms) = 
  locationFromDistance . sum $ map lengthFromMovement ms 
 -- }}}

-- ** Accessors -- {{{
positionFromLocation :: (RealFloat a) => Path a -> PathLocation a -> Point a 
positionFromLocation (Path s []) _ = s
positionFromLocation (Path s (m:ms)) loc = -- {{{
  if len <= 0 then
    s 
  else
    if mlen >= len then
      s + (positionFromMovementDistance m len)
    else
      (positionFromLocation 
        (Path end ms) 
        (locationFromDistance (len - mlen)))
  where
    len = distanceFromLocation loc
    mlen = lengthFromMovement m
    end = pathEndPosition (Path s [m])  -- }}}

distanceFromLocation :: (RealFloat a) => PathLocation a -> a 
distanceFromLocation = getDistance 
 -- }}}
 
-- ** Cutting Paths -- {{{
cutPathAt :: (RealFloat a) => Path a -> PathLocation a -> (Path a, Path a) 
cutPathAt (Path s []) _ = (Path s [], Path s [])
cutPathAt (Path s ((m@(PathMovement _ _)):ms)) d =  -- {{{
  if (distanceFromLocation d) < (lengthFromMovement m) then
    let
      (m1, m2) = cutFromMovementDistance m (distanceFromLocation d)
      [mv, m1v] = map vectorFromMovement [m, m1]
    in
      ( Path s [m1]
      , 
        concatPaths 
          [ Path (s + m1v) [m2]
          , Path (s + mv) ms ] )
  else
    let
      mv = vectorFromMovement m
      newD = locationFromDistance ((distanceFromLocation d) - (lengthFromMovement m))
      (p1, p2) = cutPathAt (Path (s + mv) ms) newD
    in
      ( concatPaths [(Path s [m]), p1]
      , p2 )   -- }}}

allPairs :: [a] -> [b] -> [(a, b)] 
allPairs xs ys = [(a, b) | a <- xs , b <- ys]
 -- }}}
 
-- * PositionMovement
-- ** Constructors -- {{{
data PositionMovement a = PositionMovement (Vector a) (PathMovement a)

pathToPositionMovements :: (RealFloat a) => Path a -> [PositionMovement a] 
pathToPositionMovements (Path _ []) = []
pathToPositionMovements (Path s (m:ms)) =
  (:)
    (PositionMovement s m)
    (pathToPositionMovements (Path (s + vectorFromMovement m) ms)) 
 -- }}}

-- * Path Intersection -- {{{
intersectPositionMovements 
  :: (RealFloat a) => PositionMovement a -> PositionMovement a -> [Point a] 
intersectPositionMovements 
  (PositionMovement s1 m1) (PositionMovement s2 m2) =
  map (+ s1) $ intersectMovementPositionMovement m1 (vectorFromTo s1 s2) m2 

intersectPathPath :: (RealFloat a) => Path a -> Path a -> [Point a] 
intersectPathPath p1 p2 =
  let [sm1, sm2] = map pathToPositionMovements [p1, p2]
  in concatMap (uncurry intersectPositionMovements) $ allPairs sm1 sm2 

intersectPMLine :: (RealFloat a) => PositionMovement a -> Line a -> [Point a] 
intersectPMLine (PositionMovement p m) l =
  intersectPositionMovementLine p m l 

intersectPathLine :: (RealFloat a) => Path a -> Line a -> [Point a] 
intersectPathLine p l =
  let
    pms = pathToPositionMovements p -- >_<
  in
    concatMap (flip intersectPMLine l) pms 

intersectPMRay :: (RealFloat a) => PositionMovement a -> LineRay a -> [Point a] 
intersectPMRay (PositionMovement p m) r =
  intersectPositionMovementRay p m r 

intersectPathRay :: (RealFloat a) => Path a -> LineRay a -> [Point a] 
intersectPathRay p r =
  let
    pms = pathToPositionMovements p
  in
    concatMap (flip intersectPMRay r) pms 

intersectPMCircle :: (RealFloat a) => PositionMovement a -> Circle a -> [Point a] 
intersectPMCircle (PositionMovement p m) c =
  intersectPositionMovementCircle p m c 

intersectPathCircle :: (RealFloat a) => Path a -> Circle a -> [Point a] 
intersectPathCircle p c =
  concatMap (flip intersectPMCircle c) $ 
    pathToPositionMovements p

projectPathPosition :: (RealFloat a) => Path a -> Point a -> Point a  
projectPathPosition path p =
  positionFromLocation path $ projectPathLocation path p 

projectOnPositionMovementDistance ::  (RealFloat a) => PositionMovement a -> Point a -> a 
projectOnPositionMovementDistance (PositionMovement p m) v =
  projectDistanceFromMovementPoint m (vectorFromTo p v) 

projectPathLocation ::  (RealFloat a) => Path a -> Point a -> PathLocation a 
projectPathLocation (Path _ []) _ = locationFromDistance 0
projectPathLocation (Path s [m]) p =
  locationFromDistance $
    projectDistanceFromMovementPoint m (vectorFromTo s p)
projectPathLocation path@(Path s (m:ms)) p = -- {{{
  let
    path1 = (Path s [m])
    path2 = (Path (s + (vectorFromMovement m)) ms)
    (d1, d2) =
      ( projectPathLocation path1 p
      , locationFromDistance ((distanceFromLocation $ projectPathLocation path2 p) + (lengthFromMovement m)) )
    objective =
      lengthFromVector . (vectorFromTo p) . (positionFromLocation path)
  in
    minimumBy (on compare objective) [d1, d2]  -- }}} 
 -- }}}
