import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified EuclideanThings as ET
import EuclideanGloss

type World = ET.Vector

main = 
  --display mydisplay white (displayWorld (ET.Vector 500 500))
  play mydisplay (white) framerate init displayWorld eventhandle timehandle 
  where
    mydisplay      = InWindow "test" (600, 600) (50, 50)
    framerate    = 50
    init         = (ET.Vector 0 0)

displayWorld :: ET.Point -> Picture
displayWorld v =
  let
    s1 = initialFromV v
    replace1 = replacen 1 s1
    replace2 = concatMap replacement replace1
    replace3 = replacen 3 s1
    replace10 = replacen 9 s1
  in
    Pictures $
      [ Pictures $
         map (drawBallSegmentBall . pairToSegment) s1 
      , (Color black) $ 
        Pictures . (map (drawBallSegmentBall . pairToSegment)) $ replace1
      , (Color red) $ 
        Pictures . (map (drawSegment . pairToSegment)) $ replace2
      , (const Blank) $ (Color red) $
        Pictures . (map (drawSegment . pairToSegment)) $ replace3 
      , (Color blue) $ 
        Pictures . (map (drawSegment . pairToSegment)) $ replace10 ]

segmentToPair :: ET.Segment -> (ET.Point, ET.Point) 
segmentToPair (ET.LineSegment p1 p2) = (p1, p2)
segmentToPair _ = undefined

pairToSegment :: (ET.Point, ET.Point) -> ET.Segment
pairToSegment (p1, p2) = ET.LineSegment p1 p2

initialFromV :: ET.Point -> [(ET.Point, ET.Point)]
initialFromV v =
  [((-v), v)]

replacen :: Int -> [(ET.Point, ET.Point)] -> [(ET.Point, ET.Point)]
replacen i =
   (foldl (.) id) $ (take i (repeat $ concatMap replacement))

replacement :: (ET.Point, ET.Point) -> [(ET.Point, ET.Point)]
replacement (p1, p2) =
  let
    m = ET.scaleVector (1/2) (p1 + p2)
    v = p2 - p1
    rotv = ET.rotateVector (ET.degreeToAngle (-90)) v
    p3 = (ET.scaleVector (1/3) v) + (ET.scaleVector (1/4) rotv) + p1
    p4 = (ET.scaleVector (2/3) v) + (ET.scaleVector (-1/4) rotv) + p1
  in
    [(p1, p3), (p3, p4), (p4, p2)]

eventhandle :: Event -> World -> World
eventhandle (EventMotion p) = const $ pointToVector p
eventhandle _ = id 

timehandle :: Float -> World -> World
timehandle = (flip const) . id
