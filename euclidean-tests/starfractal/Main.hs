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
    replace10 = replacen 6 s1
  in
    Pictures $
      [ (const Blank) $ Pictures $
         map (drawBallSegmentBall . pairToSegment) s1 
      , (const Blank) $ (Color black) $ 
        Pictures . (map (drawBallSegmentBall . pairToSegment)) $ replace1
      , (const Blank) $ (Color blue) $ 
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
  let
    [p1, p2, p3] = map ((flip ET.rotateVector) v . ET.degreeToAngle) [0, (360/3), (2*360/3)]
  in
    [(p1, p2), (p2, p3), (p3, p1)]

replacen :: Int -> [(ET.Point, ET.Point)] -> [(ET.Point, ET.Point)]
replacen i =
   (foldl (.) id) $ (take i (repeat $ concatMap replacement))

replacement :: (ET.Point, ET.Point) -> [(ET.Point, ET.Point)]
replacement (p1, p2) =
  let
    m = ET.scaleVector (1/2) (p1 + p2)
    d = ET.vectorLength (p2 - p1)
    p4 = ((ET.scaleVector (1/3)) (p2 - p1)) + p1
    p5 = ((ET.scaleVector (2/3)) (p2 - p1)) + p1
    a = ET.vectorToAngle $ p2 - p1
    [p3] = 
      ET.intersectLineLine
        (ET.lineFromPointAngle p4 (a - 60))
        (ET.lineFromPointAngle p5 (a + 60))
  in
    [(p1, p4), (p4, p3), (p3, p5), (p5, p2)]
  

eventhandle :: Event -> World -> World
eventhandle (EventMotion p) = const $ pointToVector p
eventhandle _ = id 

timehandle :: Float -> World -> World
timehandle = (flip const) . id
