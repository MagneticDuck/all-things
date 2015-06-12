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
    line1 = ET.lineFromPointPoint v (ET.nullVector)
    circle1 = ET.circleFromPointRadius v 150
    circle2 = ET.circleFromPointRadius (ET.Vector 0 0) 200
    intersect1 = ET.intersectCircleCircle circle1 circle2
  in
    Pictures $
      [ drawCircle circle1
      , drawCircle circle2
      , drawPoint ET.nullVector
      , drawPoint v
      , (Color red) $ Pictures . (map drawPoint) $ intersect1
      ,
        case intersect1 of
          [p1, p2] ->
            let
              line2 = ET.lineFromPointPoint p1 p2
              intersect2 = ET.intersectLineLine line1 line2
              tangent1 = ET.lineFromPointAngle p
            in
              Pictures
                [ drawVector p1
                , drawVector p2
                , (Color red) $ drawManualSegment v p1
                , (Color red) $ drawManualSegment v p2
                ]
          _ -> Blank
      ]

eventhandle :: Event -> World -> World
eventhandle (EventMotion p) = const $ pointToVector p
eventhandle _ = id 

timehandle :: Float -> World -> World
timehandle = (flip const) . id
