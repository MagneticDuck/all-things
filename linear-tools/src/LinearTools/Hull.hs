module LinearTools.Hull 
  ( Hull (..)
  , LineSegment(..)
  , hullToLines
  , closestToHull
  , intersectHulls ) where

import LinearTools.Vectors
import LinearTools.Line

import Data.List
import Data.Ord
import Data.Maybe

import Control.Applicative -- >;0

data Hull = SingleHull [Point] | SeperateHulls [Hull] 

breakChunks :: [a] -> [(a, a)]
breakChunks [] = []
breakChunks [x] = [(x, x)]
breakChunks (x:y:xs) = (x, y) : (breakChunks (y:xs))

hullToLines :: Hull -> [LineSegment]
hullToLines (SingleHull ps) = 
  case ps of
    [] -> []
    [p] -> [LineSegment p p]
    [p1, p2] -> [LineSegment p1 p2]
    ps -> 
      (LineSegment (head ps) (last ps)) : 
        (map (uncurry LineSegment) $ breakChunks ps)
hullToLines (SeperateHulls hs) =
  concatMap hullToLines hs

closestToHull :: Hull -> Point -> Point
closestToHull hull p =
  (minimumBy . comparing) (vectorLength . (\ap -> ap - p)) ps
  where
    ps = map (flip closestToLineSegment $ p) $ hullToLines hull

intersectHulls :: Hull -> Hull -> [Point]
intersectHulls hull1 hull2 =
  (concatMap maybeToList) . concat $
    map (\seg -> map (intersectLineSegments seg) segments1) segments2
  where
    [segments1, segments2] = map hullToLines [hull1, hull2]

arcHull :: Int -> Point -> Vector -> Vector -> Float -> Hull
arcHull n p fromLim toLim rad =
  let
    rn = fromIntegral n
    arcLength = (vectorAngle fromLim) - (vectorAngle fromLim)
    iToAngle a = 
      (vectorAngle fromLim) + arcLength - ((((fromIntegral a) * pi) / rn))
    angles = map iToAngle [0..n]
  in
    SingleHull . map (+ p) . map (scaleVector rad) . map (\a -> (Vector (cos a) (sin a))) $ 
      angles
