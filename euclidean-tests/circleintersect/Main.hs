import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified EuclideanThings as ET
import EuclideanGloss

type World = ET.Vector

main = 
  --play display (white) framerate init displayWorld eventhandle timehandle 
  animate display black displayTime
  where
    display      = InWindow "test" (600, 600) (50, 50)
    framerate    = 50
    init         = (ET.Vector 0 0)

displayTime :: Float -> Picture
displayTime =
  displayWorld . (+ (ET.Vector (-100) 0)) . (ET.scaleVector 200) . ET.angleToVector . ET.radToAngle . (/4) . realToFrac

displayWorld :: ET.Point -> Picture
displayWorld v =
  let
    vline = (ET.lineFromPointPoint v ET.nullVector)
    vcircle = (ET.circleFromPointRadius v 170)
    circle1 = (ET.circleFromPointRadius ET.nullVector 150)
    intersections = (ET.intersectCircleCircle vcircle circle1)
  in
    Pictures
      [ (Color blue) $ drawCircle vcircle
      , (Color white) $ drawVector v
      , (Color white) $ drawPoint ET.nullVector
      , (Color red) $ drawCircle circle1
      , (Color blue) . Pictures . (map drawVector) $ intersections
      , (Color blue) . Pictures . (map (drawManualSegmentBall v)) $ intersections 
      ,
        case intersections of
          [p1, p2] ->
            let
              pline = ET.lineFromPointPoint p1 p2
              intersections2 = ET.intersectLineLine pline vline
              p1tangent = ET.lineFromPointAngle p1 (ET.applyAngle (+90) $ ET.vectorToAngle (v - p1))
              p2tangent = ET.lineFromPointAngle p2 (ET.applyAngle (+90) $ ET.vectorToAngle (v - p2))
              p12tangent = ET.lineFromPointAngle p1 (ET.applyAngle (+90) $ ET.vectorToAngle (ET.nullVector - p1))
              p22tangent = ET.lineFromPointAngle p2 (ET.applyAngle (+90) $ ET.vectorToAngle (ET.nullVector - p2))
              tangentIntersections = ET.intersectLineLine p1tangent p2tangent
              tangent2Intersections = ET.intersectLineLine p12tangent p22tangent
            in
              Pictures
                [ --(Color red) . Pictures . (map drawPoint) $ intersections2 
                --, (Color red) $ drawSegment p1 p2 
                (Color blue) $ drawLine p1tangent
                , (Color blue) $ drawLine p2tangent
                , (Color red) $ drawLine p12tangent
                , (Color red) $ drawLine p22tangent
                , (Color white) $ Pictures . (map drawPoint) $ tangentIntersections
                , (Color white) $ Pictures . (map drawPoint) $ tangent2Intersections
                ,
                  case (tangentIntersections, tangent2Intersections) of
                    ([p11], [p12]) -> (Color white) $ drawLine (ET.lineFromPointPoint p11 p12)
                    _ -> Blank
                ]
          _ -> Blank
      ]

      

eventhandle :: Event -> World -> World
eventhandle (EventMotion p) = const $ pointToVector p
eventhandle _ = id 

timehandle :: Float -> World -> World
timehandle = (flip const) . id
