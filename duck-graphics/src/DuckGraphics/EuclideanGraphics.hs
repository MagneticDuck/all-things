-- module description -- {{{
{- |
  This module exports a host of methods for drawing "EuclideanThings"
  geometric constructs as 'DuckGraphics.Display.GraphicsElement's.
-} -- }}}
module DuckGraphics.EuclideanGraphics
  ( 
  -- * EuclideanThings types
    DCircle
  , DLine
  , DAngle
  -- * Drawing Primitives
  , drawCircle
  , drawFilledCircle 
  , drawLine ) where

import DuckGraphics.Display
import EuclideanThings as E

-- * EuclideanThing types -- {{{
-- | A "EuclideanThings" circle, using Doubles to store numeric values
type DCircle = E.Circle Double

-- | A "EuclideanThings" line, using Doubles to store numeric values
type DLine = E.Line Double

-- | A "EuclideanThings" angle, using Doubles to store numeric values
type DAngle = E.Angle Double

verticiesFromCircle :: Int -> DCircle -> [DVector] 
verticiesFromCircle step cir =
  map (E.liftVector realToFrac . E.transformPoint trans) unitCir
  where
    unitCir = 
      map 
        (E.vectorFromAngle . E.angleFromDegree . (* (360 / fromIntegral step)) . fromIntegral)
        [(0 :: Int), 1 .. step]
    trans =
      E.transformFromCompose
        [ E.transformFromScale (E.radiusFromCircle cir)
        , E.transformFromTranslate (E.centerFromCircle cir) ] 
 -- }}}
 
-- * Drawing Primitives -- {{{
-- | Draws an outline of the supplied circle with the 
-- supplied 'DrawStyle'
drawCircle:: DrawStyle -> DCircle -> GraphicsElement 
drawCircle style =
  LineElement style . (\verts -> verts ++ [head verts]) . verticiesFromCircle 24 

-- Draw the supplied circle with the supplied drawing style,
-- coloring in its interior with the 
drawFilledCircle :: DrawStyle -> DCircle -> GraphicsElement 
drawFilledCircle style = PolygonElement style . verticiesFromCircle 24 

-- Draws a line, or at least the part of a line inside a
-- supplied limiting circle
drawLine :: DrawStyle -> DCircle -> DLine -> GraphicsElement 
drawLine style circle line = 
  case E.intersectLineCircle line circle of
    [p1, p2] ->
      LineElement style [p1, p2]
    _ -> OverlayElement [] 
    -- }}}
