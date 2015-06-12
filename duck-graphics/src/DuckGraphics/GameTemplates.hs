-- docs -- {{{
{- |
  This module provides some additional templates (substitutes for
  the 'baseGameDef' exported from "DuckGraphics.GLUTGame")
-} -- }}}
module DuckGraphics.GameTemplates 
  ( interactPointGameTemplate ) where

import DuckGraphics.Display
import DuckGraphics.GLUTGame
-- import DuckGraphics.EuclideanGraphics

import EuclideanThings as E

-- import Data.Functor ((<$>))

-- | This defines an application whose state is the current position
interactPointGameTemplate :: (DVector -> IO ()) -> GameDefinition DVector 
interactPointGameTemplate g =
  (gameDefinition E.nullVector)
    { gameDisplay = myGameDisplay
    , gameEvent = myGameEvent }
  where
    myGameDisplay v =
      (>> return v) $ g v
    myGameEvent event v =
      case event of
        (GameMotionEvent nv) -> return nv
        _ -> return v  
