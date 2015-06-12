import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified EuclideanThings as ET
import EuclideanGloss

type World = ET.Vector

main = 
  animate display black displayAnimate
  --play display (white) framerate init displayWorld eventhandle timehandle 
  where
    display      = InWindow "test" (600, 600) (50, 50)
    framerate    = 50
    init         = (ET.Vector 0 0)

getcircle :: ET.Angle -> ET.Circle
getcircle a =
  let
    line1 = ET.lineFromPointAngle ET.nullVector 0
    line2 = ET.lineFromPointAngle (ET.Vector 100 0) a
    intersect1 = ET.intersectLineLine line1 line2
  in
    case intersect1 of
      [] -> ET.Circle ET.nullVector 0
      _ ->
        ET.circleFromPointPointPoint (ET.Vector 100 0) (ET.Vector (-100) 0) (head intersect1)

transformPoint :: ET.Angle -> ET.Point -> ET.Point
transformPoint a p =
  case a of
    180 -> p
    0 -> p
    _ ->
      let
        circle = getcircle a
        project = ET.projectOnCircle circle p
      in
        project + (ET.vectorFromTo p project)

displayAnimate :: Float -> Picture
displayAnimate t =
  let
    v = ET.rotateVector ((ET.radToAngle . realToFrac . (/5)) t) (ET.Vector 100 0)
    a = ET.vectorToAngle v
    circle = getcircle a
    v1 = ET.projectOnCircle circle v 
    angles = [0, (1/15) .. 360]
    prereflect = 
      map (\a -> ((ET.scaleVector 100) $ ET.angleToVector (ET.degreeToAngle a))) $ angles
    prepath = ET.pathFromPoints prereflect
    postpath = ET.pathFromPoints $ map (transformPoint a) prereflect
  in
    Pictures
      [ --drawVector v
      --, drawCircle circle 
      --, drawPath prepath
      (Color red) $ drawPath postpath ]
      --, drawVector v1]

displayWorld :: ET.Point -> Picture
displayWorld v =
  let
    circle = getcircle a
    a = ET.vectorToAngle v
    angles = [0, (1/10) .. 360]
    prereflect = 
      map (\a -> ((ET.scaleVector 100) $ ET.angleToVector (ET.degreeToAngle a))) $ angles
    prepath = ET.pathFromPoints prereflect
    postpath = ET.pathFromPoints $ map (transformPoint a) prereflect
  in
    Pictures
      [ 
      drawCircle circle
      , drawPath prepath
      , drawVector v 
      , (Color red) $ drawPath postpath 
      , Text $ show a]
      

eventhandle :: Event -> World -> World
eventhandle (EventMotion p) = const $ pointToVector p
eventhandle _ = id 

timehandle :: Float -> World -> World
timehandle = (flip const) . id
