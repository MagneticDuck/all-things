import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified EuclideanThings as ET
import EuclideanGloss


type WorldMovement = Double
type World = (Double, WorldMovement, ET.Point)

main = 
  play display (white) framerate init displayWorld eventhandle timehandle 
  where
    display      = InWindow "test" (600, 600) (50, 50)
    framerate    = 50
    init         = (0, 0, ET.nullVector)

displayWorld :: World -> Picture
displayWorld (x, _, v) =
  Pictures
    [ drawVector v
    , Text $ (show . round) x ]

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
