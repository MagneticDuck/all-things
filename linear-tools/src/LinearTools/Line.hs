module LinearTools.Line
  ( LineSegment(..)
  , closestToLineSegment
  , intersectLineSegments ) where

import LinearTools.Vectors
import Data.List
import Data.Ord

-- Line a b c represents the locus in (x, y) <- R^2 of ax + by + c == 0
data Line = Line Point Point

closestToLine :: Line -> Point -> Point
closestToLine (Line va@(Vector ax ay) vb@(Vector bx by)) (Vector cx cy) =
  let
    l = vectorLength (va - vb)
    r =
      (((ay-cy)* (ay-by)) - ((ax-cx) * (bx-ax))) / (l * l)
  in
    Vector (ax + (r * (bx - ax))) (ay + (r * (by - ay)))

isOnLine :: Line -> Point -> Bool
isOnLine (Line va vb) (Vector x y) =
  let
    StandardLine a b c = pointsToStandardLine va vb
  in
    (a * x) + (b * y) + c == 0

data StandardLine = StandardLine Float Float Float
  
pointsToStandardLine :: Point -> Point -> StandardLine
pointsToStandardLine (Vector x1 y1) (Vector x2 y2)
  | x1 == x2 = StandardLine 1 0 (-x1)
  | y1 == y2 = StandardLine 0 1 (-y1)
  | otherwise =
      let m = (x1 - x2) / (y1 - y2) in
        StandardLine 1 (-m) (y1 - m * x1)

lineToStandardLine :: Line -> StandardLine
lineToStandardLine (Line va vb) = pointsToStandardLine va vb

intersectLines :: Line -> Line -> Maybe Point
intersectLines 
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
          Just $ Vector (ax + (r * (bx-ax))) (ay+(r * (by-ay)))
          
data LineSegment = LineSegment Point Point

segmentToLine :: LineSegment -> Line
segmentToLine (LineSegment va vb) = Line va vb

isOnSegment :: LineSegment -> Point -> Bool
isOnSegment segment@(LineSegment va vb) p =
  isOnLine (Line va vb) p && (inBox segment p)

inBox :: LineSegment -> Point -> Bool
inBox (LineSegment (Vector x1 y1) (Vector x2 y2)) (Vector x y) =
  inBox
  where
    maxX = maximum [x1, x2]
    minX = minimum [x1, x2]
    maxY = maximum [y1, y2]
    minY = minimum [y1, y2]
    inBox =
      and $
        [ x >= minX
        , x <= maxX
        , y >= minY
        , y <= maxY ]

closestToLineSegment :: LineSegment -> Point -> Point
closestToLineSegment seg@(LineSegment va vb) p =
  let
    lineP = closestToLine (Line va vb) p
    distToP = vectorLength . ((-) p)
  in
    if (isOnSegment seg lineP) then
      lineP
    else
      (minimumBy . comparing) distToP [va, vb]

intersectLineSegments :: LineSegment -> LineSegment -> Maybe Point
intersectLineSegments segment1 segment2 =
  let
    p = intersectLines (segmentToLine segment1) (segmentToLine segment2)
  in
    case p of
      Nothing -> Nothing
      Just ap ->
        if (inBox segment1 ap) && (inBox segment2 ap) then
          Just ap
        else
          Nothing
