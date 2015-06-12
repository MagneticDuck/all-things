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
    path1 = ET.applyPath (ET.scaleVector 100) $ ET.pathFromPolygon 7
    path2 = ET.applyPath ((+v) . (ET.rotateVector (ET.degreeToAngle 20))) path1
    intersect1 = ET.intersectPaths path1 path2
  in
    Pictures
      [ (Color red) $ drawPath path1 
      , (Color blue) $ drawPath path2
      , (Color magenta) . Pictures . (map drawPoint) $ intersect1 ]

eventhandle :: Event -> World -> World
eventhandle (EventMotion p) = const $ pointToVector p
eventhandle _ = id 

timehandle :: Float -> World -> World
timehandle = (flip const) . id
