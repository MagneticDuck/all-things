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
    p1 = (ET.Vector 100 20)
    path2 = ET.arcPathFromCircleAngleAngle (ET.circleFromPointRadius v 100) (ET.degreeToAngle 0) (ET.degreeToAngle 90)
    path1 = ET.appendPathToPath (ET.linePathFromPointPoint ET.nullVector v) path2
    line1 = ET.lineFromPointAngle (ET.Vector 100 100) (ET.degreeToAngle 20)
    intersect1 = ET.intersectPathLine path1 line1
  in
    Pictures
      [ drawBallPath path1
      , drawPoint p1
      , drawPoint ET.nullVector
      , drawLine line1
      , (Color red) $ Pictures . (map drawPoint) $ intersect1 ]
  

eventhandle :: Event -> World -> World
eventhandle (EventMotion p) = const $ pointToVector p
eventhandle _ = id 

timehandle :: Float -> World -> World
timehandle = (flip const) . id
