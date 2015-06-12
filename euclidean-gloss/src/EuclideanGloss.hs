module EuclideanGloss
  (translateByVector
  , vectorToPoint
  , pointToVector
  , drawPoint
  , drawVector
  , drawManualSegment
  , drawManualSegmentBall
  , drawManualBallSegmentBall
  , drawLine
  , drawRay
  , drawCircle
  , drawMovement
  , drawPath
  , concatDraw) where

import qualified EuclideanThings as ET
import Graphics.Gloss

translateByVector :: 
  (RealFloat a) =>
    (ET.Vector a) -> Picture -> Picture
translateByVector (ET.Vector x y) = Translate (realToFrac x) (realToFrac y)

vectorToPoint :: (RealFloat a) => (ET.Vector a) -> Point
vectorToPoint (ET.Vector x y) = ((realToFrac x), (realToFrac y))

pointToVector :: (RealFloat a) => Point -> (ET.Vector a)
pointToVector (x, y) = ET.Vector (realToFrac x) (realToFrac y)

drawPoint :: (RealFloat a) => (ET.Point a) -> Picture
drawPoint v =
  Pictures
    [ translateByVector v $ ThickCircle (realToFrac r) (realToFrac ri) 
    , Line 
        [ (vectorToPoint $ v + (ET.Vector r 0))
        , (vectorToPoint $ v + (ET.Vector (-r) 0)) ]
    , Line 
        [ (vectorToPoint $ v + (ET.Vector 0 r)) 
        , (vectorToPoint $ v + (ET.Vector 0 (-r)))]
    ]
    where
      r = 7
      ri = 3

drawVector :: (RealFloat a) => (ET.Vector a) -> Picture
drawVector v =
  Pictures 
    [ drawPoint v
    , Line [(0, 0), (vectorToPoint v)] ]

drawManualSegment :: 
  (RealFloat a) =>
    (ET.Point a) -> (ET.Point a) -> Picture
drawManualSegment v1 v2 =
  Line [(vectorToPoint v1), (vectorToPoint v2)]

drawManualSegmentBall :: 
  (RealFloat a) =>
    (ET.Point a) -> (ET.Point a) -> Picture
drawManualSegmentBall v1 v2 =
  Pictures 
    [ drawManualSegment v1 v2
    , drawPoint v2]

drawManualBallSegmentBall :: 
  (RealFloat a) =>
    (ET.Point a) -> (ET.Point a) -> Picture
drawManualBallSegmentBall v1 v2 =
  Pictures
    [ drawPoint v1
    , drawManualSegment v1 v2
    , drawPoint v2]

drawLine :: (RealFloat a) => (ET.Line a) -> Picture
drawLine l =
  let
    bounding = ET.circleFromPointRadius ET.nullVector 3000
    ps = ET.intersectLineCircle l bounding
  in
    case ps of
      [p1, p2] ->
        drawManualSegment p1 p2
      _ -> Blank

drawRay :: (RealFloat a) => (ET.LineRay a) -> Picture
drawRay r =
  let
    bounding = ET.circleFromPointRadius ET.nullVector 3000
    ps = ET.intersectRayCircle r bounding
  in  
    case ps of
      [p] ->
        drawManualSegment (ET.startFromRay r) p
      _ -> Blank
  

drawCircle :: (RealFloat a) => (ET.Circle a) -> Picture
drawCircle (ET.Circle c r) =
  translateByVector c $ Circle (realToFrac r)

drawMovement :: (RealFloat a) => (ET.PathMovement a) -> Picture
drawMovement m =
  let
    ml = ET.lengthFromMovement m
    points = map ((ET.positionFromMovementDistance m) . fromIntegral) [(0 :: Int), 5 .. (round ml)]
  in
    Line . concat$ 
      [ [vectorToPoint ET.nullVector]
      , map vectorToPoint points
      , [vectorToPoint (ET.positionFromMovementDistance m (ET.lengthFromMovement m)) ] ]

drawPath :: (RealFloat a) => (ET.Path a) -> Picture
drawPath (ET.Path _ []) = Blank 
drawPath (ET.Path s (m:ms)) = 
  Pictures
    [ translateByVector s $ drawMovement m
    , drawPath (ET.Path (s + (ET.vectorFromMovement m)) ms) ]

concatDraw :: (a -> Picture) -> [a] -> Picture
concatDraw f = Pictures . (map f)

