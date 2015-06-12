-- module description -- {{{
{- |
  Module : DuckGraphics.Display
  
  This module makes drawing graphics to the screen inside an OpenGL session
  pretty easy, and defines "EuclideanThings" transformations to transform
  between the graphics world (points here are clamped to the range from (0, 0)
  to the specified AspectRatio) and the OpenGL world.
  
  The transformation used transforms the frame of points from the graphics
  world to the largest frame of points in the OpenGL world while maintaining
  aspect ratio and taking care to center the resultant frame on the screen
  as best as possible.
  
  This approach is suitable for vector graphics and not much else. As goes
  without saying, I'm going to use exclusively simple vector graphics in my game.
-} -- }}}
module DuckGraphics.Display 
-- exports -- {{{
  ( DVector

  -- * Colors
  , Color4(..)
  , DColor4
  , realToFracColor4
  -- ** Color constants
  , colorWhite
  , colorBlack
  , colorRed
  , colorGreen
  , colorBlue

  -- * Graphics
  -- ** DrawStyle
  , DrawStyle(..)
  , drawStyle
  -- ** GraphicsElement
  , GraphicsElement(..)
  , liftGraphicsElement
  -- ** AspectRatio
  , AspectRatio
  , transformToOpenGL
  , transformFromOpenGL
  -- ** drawGraphicsElement
  , drawGraphicsNoFlush 
  , drawGraphicsElementNoBorder 
  , drawGraphicsElement 
  -- ** DuckGraphics
  , DuckGraphics(..) 
  , duckGraphics
  , drawDuckGraphics ) where -- }}}

-- imports  
import Graphics.UI.GLUT
import qualified EuclideanThings as E
import Data.Functor ((<$>)) 
import Control.Monad (forM_) 

-- | I'm using "EuclideanThings" for everything geometry-related here and I'm using Double for all numerical values  
type DVector = E.Vector Double  

-- * Colors -- {{{
-- | You know, Doubles are really just nice 
type DColor4 = Color4 Double

liftColor4 :: (a -> b) -> Color4 a -> Color4 b 
liftColor4 f (Color4 a b c d) = Color4 (f a) (f b) (f c) (f d) 

-- | realToFrac lifted to Color4, useful for transforming
-- 'DColor4' into OpenGL Color4 and vice versa 
realToFracColor4 :: (Real  a, Fractional b) => Color4 a -> Color4 b 
realToFracColor4 = liftColor4 realToFrac  

-- | some color constants 
colorWhite, colorBlack, colorRed, colorGreen, colorBlue :: DColor4 
colorWhite = Color4 1 1 1 1
colorBlack = Color4 0 0 0 1
colorRed   = Color4 1 0 0 1
colorGreen = Color4 0 1 0 1
colorBlue  = Color4 0 0 1 1 
 -- }}}
 
-- * Graphics
-- ** DrawStyle -- {{{
-- | This defines alone with a list of verticies is parameter 
-- of a 'PolygonElement' or 'LineElement'  
data DrawStyle = 
  DrawStyle
    { drawColor  :: DColor4
    , drawBorder :: Double } 

-- | Default display style, with white foreground and line width of 3 pixels 
drawStyle :: DrawStyle 
drawStyle = 
  DrawStyle
    { drawColor = Color4 1 1 1 1
    , drawBorder = 3 } 
 -- }}}

-- ** GraphicsElement -- {{{
-- | Represents the graphics we want to draw. Includes all of the information
-- needed to draw them except for the aspect ratio 
data GraphicsElement =   
  PolygonElement DrawStyle [DVector] 
  | LineElement DrawStyle [DVector] 
  | OverlayElement [GraphicsElement] 

-- | Transforms all point data in a GraphicsElement, 
-- in case this should be necessary 
liftGraphicsElement :: (DVector -> DVector) -> GraphicsElement -> GraphicsElement 
liftGraphicsElement f (PolygonElement d pts) =
  PolygonElement d (map f pts)
liftGraphicsElement f (LineElement d pts) =
  LineElement d (map f pts)
liftGraphicsElement f (OverlayElement elms) =
  OverlayElement (map (liftGraphicsElement f) elms) 
 -- }}}
 
-- ** AspectRatio -- {{{
-- | Defines the range in which we can draw stuff, and provides 
-- and aspect ratio which is guarenteed to be respected 
type AspectRatio = DVector

-- | Represents a transformation that maps points in the ranger
-- defined by the 'AspectRatio' to their respective positions
-- on the window 
transformToOpenGL :: AspectRatio -> IO (E.Transform Double) 
transformToOpenGL aspect@(E.Vector aspectx aspecty) = -- {{{
  do
    (Size wiw wih) <- get windowSize
    let 
      wSize@(E.Vector wWidth wHeight) = E.Vector (fromIntegral wiw) (fromIntegral wih)
      [windowR, aspectR] = map E.slopeFromVector [wSize, aspect]
      (framex, framey) = 
        if windowR > aspectR then
          (wWidth, wWidth * aspectR)
        else
          (wHeight / aspectR, wHeight)
    return $
      E.transformFromCompose
        [ E.transformFromScale 2
        , E.transformFromTranslate (uncurry E.Vector (-1, -1))
        , E.transformFromScaleX $ framex / (aspectx * wWidth)
        , E.transformFromScaleY $ framey / (aspecty * wHeight) ]  -- }}}

-- | Simply the inverse of 'transformToOpenGL'; takes a point on the screen
-- and maps it to its corresponding position as a 'GraphicsElement' vertex 
transformFromOpenGL :: AspectRatio -> IO (E.Transform Double) 
transformFromOpenGL aspect =
  E.invertTransform <$> transformToOpenGL aspect 
 -- }}}
     
-- ** drawGraphicsElement -- {{{
drawVerticies :: (DVector -> DVector) -> [DVector] -> IO ()  
drawVerticies f verts' =
  forM_ verts
    (\(E.Vector ax ay) -> vertex $ Vertex2 ax ay)
  where
    myCon = realToFrac :: Double -> GLfloat
    verts = map (E.liftVector myCon . f) verts' 

-- | Renders a GraphicsElement to the screen without any fuss or
-- overhead (unlike 'drawGraphicsElement'). Only use this if you're
-- trying to draw a 'GraphicsElement' inside a larger graphics
-- callback for some reason.
drawGraphicsNoFlush :: (DVector -> DVector) -> GraphicsElement -> IO () 
drawGraphicsNoFlush f (PolygonElement s verts) = 
  do
    currentColor $= realToFracColor4 (drawColor s)
    lineWidth $= 0.5
    renderPrimitive Polygon $ drawVerticies f verts 
drawGraphicsNoFlush f (LineElement s verts) = 
  do
    currentColor $= realToFracColor4 (drawColor s)
    lineWidth $= realToFrac (drawBorder s)
    renderPrimitive LineStrip $ drawVerticies f verts 
drawGraphicsNoFlush aspect (OverlayElement gelems) =
  mapM_ (drawGraphicsNoFlush aspect) gelems 

drawBorders :: DColor4 -> (DVector -> DVector)-> IO () 
drawBorders bgcol f =  -- {{{
    drawGraphicsNoFlush id . OverlayElement $
      map (PolygonElement bgStyle) 
        [ myRect (E.Vector (-1) (-1)) (f (E.Vector 0 1))
        , myRect (E.Vector (-1) 1) (f (E.Vector 1 1)) 
        , myRect (E.Vector 1 1) (f (E.Vector 1 0)) 
        , myRect (E.Vector 1 (-1)) (f (E.Vector 0 0)) ]
  where
    bgStyle = drawStyle { drawColor = bgcol }
    myRect (E.Vector x1 y1) (E.Vector x2 y2) = 
      [E.Vector x1 y1, E.Vector x1 y2, E.Vector x2 y2, E.Vector x2 y1] -- }}}

type BGColor = DColor4

drawGraphicsElementInternal 
  :: ((DVector -> DVector) -> IO ()) -> BGColor -> AspectRatio -> GraphicsElement -> IO () 
drawGraphicsElementInternal option bg aspect gelem =
  do
    drawGraphicsNoFlush id $ 
      PolygonElement (drawStyle { drawColor = bg }) 
        [E.Vector (-1) (-1), E.Vector (-1) 1, E.Vector 1 1, E.Vector 1 (-1)]
    f <- E.transformPoint <$> transformToOpenGL aspect
    clear [ ColorBuffer ]
    drawGraphicsNoFlush f gelem
    option f
    swapBuffers  

-- docs -- {{{
{- |
  This draws a graphics element to the screen similar to the way
  'drawGraphicsElement' does; however, it does not draw the "borders";
  graphics that are not contained in the normal coordinate range
  (defined by the 'AspectRatio' supplied) but do map to points
  contained in the OpenGL window are drawn, whereas in the
  normal definition they are clipped.
-} -- }}}
drawGraphicsElementNoBorder :: BGColor -> AspectRatio -> GraphicsElement -> IO ()  
drawGraphicsElementNoBorder bg = (`drawGraphicsElementInternal` bg) . const $ return () 

-- docs -- {{{
{- |
  Given a background color with which to fill in the back of the screen,
  a color to use to draw the borders of the screen
  (clipping any graphics that fall off of defined range defined by the
  aspect ratio), an 'AspectRatio', and a 'GraphicsElement', this method
  draws everything to the screen.
  
  This should only be used once per display cycle because it flushes
  after it's done. For simplified rendering of individual GraphicsElements
  with no additional overhead (border clipping, flushing, buffer swapping),
  use 'drawGraphicsNoFlush' 

  Note: if you want to fill the screen with a custom color on which
  to draw graphics, supplying a background color for this method
  alone will not do much; instead, configure the color used
  to clear the screen in the "DuckGraphics.GLUTGame.GameDefinition".
-} -- }}}
drawGraphicsElement :: BGColor -> DColor4 -> AspectRatio -> GraphicsElement -> IO () 
drawGraphicsElement bg borderCol = drawGraphicsElementInternal (drawBorders borderCol) bg
 -- }}}

-- ** DuckGraphics -- {{{

-- | DuckGraphics is a one-size-fit all solution containing
-- all of the vector graphics and options needed to draw
-- the graphics to the screen once and for all!
data DuckGraphics =
  DuckGraphics
    { duckGraphicsElement    :: GraphicsElement
    , duckGraphicsBorders    :: Maybe DColor4
    , duckGraphicsBackground :: DColor4
    , duckGraphicsAspect     :: AspectRatio }

-- | The default value of DuckGraphics \-\-
-- background and borders are white, the aspect ratio is 1:1,
-- and the graphics element rendered is the empty element
-- `(OverlayElement [])`
duckGraphics :: DuckGraphics
duckGraphics =
  DuckGraphics
    { duckGraphicsElement = OverlayElement [] 
    , duckGraphicsBorders = Just $ Color4 1 1 1 1
    , duckGraphicsBackground = Color4 1 1 1 1
    , duckGraphicsAspect = E.Vector 1 1}

-- | Quite simply, this draws a DuckGraphics to the screen; this is a simplified
-- interface to drawGraphicsElement and that host of methods.
drawDuckGraphics :: DuckGraphics -> IO ()
drawDuckGraphics duckGraphics = 
  case duckGraphicsBorders duckGraphics of
    Just borderColor ->
      drawGraphicsElement backgroundColor borderColor aspect graphicsElement
    Nothing ->
      drawGraphicsElementNoBorder backgroundColor aspect graphicsElement
  where
    graphicsElement = duckGraphicsElement duckGraphics 
    backgroundColor = duckGraphicsBackground duckGraphics 
    aspect     = duckGraphicsAspect duckGraphics
 -- }}}
