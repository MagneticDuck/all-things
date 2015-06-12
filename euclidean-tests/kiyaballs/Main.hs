import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified EuclideanThings as ET
import EuclideanGloss

type World = (ET.Vector, ET.Vector, ET.Vector, [ET.Vector], Double)

main = 
  play display (white) framerate init displayWorld eventhandle timehandle 
  where
    display      = InWindow "test" (600, 600) (50, 50)
    framerate    = 50
    init         = ((ET.Vector 0 0), (ET.Vector 0 0), (ET.Vector 0 0), [], 0)

displayWorld :: World -> Picture
displayWorld (c, p, v, hs, d) =
  Pictures
    [ drawPoint c
    , (Color red) $ drawPoint p 
    , Pictures . (map drawPoint) $ hs]

eventhandle :: Event -> World -> World
eventhandle (EventMotion m) (c, p, v, hs, d) = ((pointToVector m), p, v, hs, d)
eventhandle _ (c, p, v, hs, d) = (c, p, v, hs, d)

timehandle :: Float -> World -> World
timehandle t (c, p, v, hs, d) =
  ( c
  , (p + (ET.scaleVector (realToFrac t) v))
  , (v + (ET.scaleVector (realToFrac t) f))
  , 
    if d > 100 then
      hs ++ [p]
    else
      hs
  ,
    if d > 100 then
      0
    else
      (d + (d - (realToFrac t)))
  )
      
    
  where
    u = ET.vectorFromTo p c
    d = ET.vectorLength u
    f = 
      case d of
        0 -> ET.nullVector 
        _ -> ET.scaleVector (20000/(d ^ 2)) u
    

  
