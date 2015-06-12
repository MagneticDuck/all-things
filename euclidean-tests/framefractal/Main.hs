import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified EuclideanThings as ET
import EuclideanGloss

type World = ET.Vector

main = 
  --display mydisplay white (displayWorld (ET.Vector 500 500))
  play mydisplay (white) framerate init displayWorld eventhandle timehandle 
  where
    mydisplay      = InWindow "test" (600, 600) (50, 50)
    framerate    = 50
    init         = (ET.Vector 0 0)

everyother :: [a] -> [a]
everyother [] = []
everyother [a] = []
everyother (x:(y:xs)) = x : (everyother xs)

rotatelist :: Int -> [a] -> [a]
rotatelist i xs = (drop i xs) ++ (reverse $ take i $ reverse xs)

allpairs :: [a] -> [(a, a)]
allpairs [] = []
allpairs [a] = []
allpairs (x:xs) = (zip (repeat x) xs) ++ (allpairs xs)

displayWorld :: ET.Point -> Picture
displayWorld v =
  let
    s1 = initialFromV 7 v
    s10 = buildPolys 150 s1
    rootpoly = everyother $ last s10
    rootstar = map (ET.scaleVector (9/10)) $ concat $ map (\(a, b) -> [a, b]) $ allpairs rootpoly
  in
    Pictures
      [ (Color blue) . Pictures . (map drawPoly) $ s10
      , (Color blue) $ drawPoly s1
      , (const Blank) $ (Color red) $ drawPoly rootstar]

type APolygon = [ET.Point]

drawPolyBalls :: APolygon -> Picture
drawPolyBalls poly = 
  Pictures $ (zipWith drawManualSegmentBall poly ((last poly) : (init poly)))

drawPoly :: APolygon -> Picture
drawPoly poly =
  Pictures $ (zipWith drawManualSegment poly ((last poly) : (init poly)))

initialFromV :: Double -> ET.Point -> APolygon
initialFromV i v =
  let
    regpol =
      zipWith ET.rotateVector (map ET.degreeToAngle [0, (360 / i) .. (360 - (360 / i))]) (repeat v)
    smallpol = map ((ET.rotateVector (ET.degreeToAngle $ (360 / (2 * i)))) . (ET.scaleVector (1/7))) regpol
    shuffle xs ys = concat $ zipWith (\x y -> [x, y]) xs ys
  in
    shuffle regpol smallpol
    

buildPolys :: Int -> APolygon -> [APolygon]
buildPolys i s 
  | i <= 0 = [s]
  | otherwise =
      (nextPoly s) : (buildPolys (i - 1) (nextPoly s))

nextPoly :: APolygon -> APolygon
nextPoly poly =
  zipWith myScale poly ((last poly) : (init poly))
  where
    myScale pa pb = (ET.scaleVector (1/12) (pb - pa)) + pa

eventhandle :: Event -> World -> World
eventhandle (EventMotion p) = const $ pointToVector p
eventhandle _ = id 

timehandle :: Float -> World -> World
timehandle = (flip const) . id
