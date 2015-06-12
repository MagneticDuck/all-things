import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified EuclideanThings as ET
import EuclideanGloss
import Data.Fixed

type WorldMovement = Double
type World = (Double, WorldMovement, ET.Point)

main = 
  play display black framerate init displayWorld eventhandle timehandle 
  where
    display      = InWindow "test" (600, 600) (50, 50)
    framerate    = 50
    init         = (0, 0, ET.nullVector)

displayWorld :: World -> Picture
displayWorld (x, _, v) =
  let
    ps =
      map (ET.arcPathFromPointPointHeight (ET.Vector (-200) (-200)) (ET.Vector 200 200)) [0, 0.01 .. 0.5]
    maxdistance = ET.distanceFromLocation $ ET.maxLocation (last ps)
    loc = ET.locationFromDistance ((mod' x (maxdistance / 50)) * 50)
    poses =
      map (flip ET.positionFromLocation loc) ps
    whip = ET.pathFromPoints poses
    whips = map (\a -> ET.transformPath (ET.transformFromRotate a) whip) (map ET.degreeToAngle [0, (360 / n) .. 360 ])
    n = 50
  in
  Pictures
    [ --Pictures . (map drawPath) $ ps
    --drawPath whip ]
    (Color white) $ Pictures . (map drawPath) $ whips ]


eventhandle :: Event -> World -> World
eventhandle (EventKey k Down _ _) (p, m, v) =
  case k of
    (Char 'w') -> (p, 1, v)
    (Char 's') -> (p, (-1), v)
    _ -> (p, m, v)
eventhandle (EventKey k Up _ _) (p, m, v) =
  case k of
    (Char 'w') -> (p, 0, v)
    (Char 's') -> (p, 0, v)
    _ -> (p, m, v)
eventhandle (EventMotion nv) (p, m, v) =
  (p, m, (pointToVector nv))

timehandle :: Float -> World -> World
timehandle t (p, m, v) =
  let
    f = (* 2)
  in
    case signum m of
      1 -> ((p + (realToFrac (f t))), m, v)
      (-1) -> ((p - (realToFrac (f t))), m, v)
      0 -> (p, m, v)
