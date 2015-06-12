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
    circle = ET.circleFromPointRadius ET.nullVector 100 
    [a1, a2] = [(ET.degreeToAngle 0), (ET.vectorToAngle v)]
    p1 = 
      (ET.transformPath (ET.transformFromTranslate v) $
        (ET.linePathFromPointPoint ET.nullVector (ET.Vector 100 100)))
    line1 = ET.lineFromPointPoint (ET.Vector 50 50) (ET.Vector 100 20)
    intersect1 = ET.intersectPathLine p1 line1 
  in
    Pictures
      [ drawPath p1 
      , drawLine line1
      --, drawCircle circle
      , Pictures . (map drawPoint) $ intersect1]

eventhandle :: Event -> World -> World
eventhandle (EventMotion p) = const $ pointToVector p
eventhandle _ = id 

timehandle :: Float -> World -> World
timehandle = (flip const) . id
