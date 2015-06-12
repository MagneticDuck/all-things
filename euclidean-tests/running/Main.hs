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
    init         = (ET.Vector 10 10)

displayWorld :: ET.Point -> Picture
displayWorld v =
  let
    intersect = ET.intersectCircleLine (ET.Circle (ET.Vector (-100) 0) 200) (ET.lineFromPointAngle ET.nullVector 0)
    tailpoint =
    p1 =
      ET.concatPaths
        [ ET.arcPathFromCircleAngleAngle (ET.Circle ET.nullVector 100) 0 180
        , ET.arcPathFromCirclePointPoint (ET.Circle (ET.Vector (-100) 0) 200) (ET.Vector 100 0) (ET.Vector 
        
  in
    Pictures 
      [ drawPoint v
      , drawPath p1 ]

eventhandle :: Event -> World -> World
eventhandle (EventMotion p) = const $ pointToVector p
eventhandle _ = id 

timehandle :: Float -> World -> World
timehandle = (flip const) . id
