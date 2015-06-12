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
displayWorld (m, _, v) =
  let
    xline = ET.lineFromPointAngle ET.nullVector 90
    yline = ET.lineFromPointAngle ET.nullVector 0
    limits =
      [ ET.Vector (-400) 0
      , ET.Vector 400 0 ]

    asymtotes =
      [ 0 ]
    domain = filter (not . ((flip elem) asymtotes)) $ [(-400), ((-400) + (1/2)) .. 400] 
    f1 x' =
      let
        x = (x' / 80)
        res =
          (m + x) / (x ^ 2)
      in
        res * 30
    f2 x' =
      let
        x = (x' / 50)
        res =
          cos x
      in
        res * 30
    fp1 = ET.pathFromPoints $ (zipWith ET.Vector) domain (map f1 domain) 
    fp2 = ET.pathFromPoints $ (zipWith ET.Vector) domain (map f2 domain) 
    xintersect1 = ET.intersectPathLine fp1 xline

    line1 =
      case xintersect1 of
        (_:_) ->
          ET.lineFromPointAngle (head xintersect1) (ET.degreeToAngle 80)
        _ -> ET.lineFromPointAngle ET.nullVector (ET.degreeToAngle 80)
    
    intersect2 = ET.intersectPathLine fp1 line1
  in
    Pictures
      [ Pictures . (map drawLine) $ [xline, yline] 
      , Pictures . (map drawPoint) $ limits
      , Pictures . (map drawPoint) $ xintersect1
      , Translate 100 200 $ Text $ (show $ round (m * 2)) ++ "/2"

      , (Color red) $ drawLine line1
      , Pictures . (map drawPoint) $ intersect2
      , (Color red) $ drawPath fp1 ]
      --, (Color blue) $ drawPath fp2  ]

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
