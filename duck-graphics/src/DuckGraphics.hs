-- module description -- {{{
{- |
  module : DuckGraphics
  
  This module is intended to make OpenGL a bit easier to use
  and act as a base for sick new game I'm making by holding all of
  the nasty OpenGL interfacing code :D
-} -- }}}
module DuckGraphics (module X) where

import DuckGraphics.GLUTGame as X
import DuckGraphics.Display as X
import DuckGraphics.EuclideanGraphics as X
import DuckGraphics.GameTemplates as X
