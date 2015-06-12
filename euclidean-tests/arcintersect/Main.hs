import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified EuclideanThings as ET
import EuclideanGloss

type World = ET.Vector

main = 
  play display (white) framerate init displayWorld eventhandle timehandle 
  where
    display      = InWindow "test" (600, 600) (50, 50)
    framerate    = 50
    init         = (ET.Vector 0 0)

displayWorld :: ET.Point -> Picture
displayWorld v =
  let
    s1 = ET.ArcSegment v 100 (ET.degreeToAngle 0) (ET.degreeToAngle 100)
    s2 = ET.ArcSegment ET.nullVector 120 (ET.degreeToAngle 0) (ET.degreeToAngle 180)
    intersect1 = ET.intersectSegments s1 s2
  in
    Pictures $
      [ drawPoint $ ET.nullVector 
      , (Color green) $ drawVector v
      , drawBallSegmentBall s1
      , drawBallSegmentBall s2
      , Pictures . (map drawPoint) $ intersect1]
    

eventhandle :: Event -> World -> World
eventhandle (EventMotion p) = const $ pointToVector p
eventhandle _ = id 

timehandle :: Float -> World -> World
timehandle = (flip const) . id
