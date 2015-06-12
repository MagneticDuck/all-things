-- module description -- {{{
{- |
  module : DuckGraphics.GLUTGame
  
  This module provides a simple scaffolding of sorts to
  simplify the OpenGL overhead involved in writing an OpenGL
  application, especially a game; it exports a 'GameDefinition'
  data type (and a default definition 'gameDefinition') with
  various callbacks and settings that the user can define.
  
  The game definition can then be run with 'runGame', requiring 
  no additional initialization to function correctly. 
-} -- }}}
module DuckGraphics.GLUTGame 
-- exports -- {{{
  ( 
  -- * GameDefinition
  GameCallback(..)
  , GameDefinition(..)
  , gameDefinition
  , GameEvent(..)
  -- * runGame
  , runGame ) where
-- }}}

-- imports  -- {{{
import Control.Applicative
import Control.Monad

import DuckGraphics.Display
import EuclideanThings as E

import Graphics.UI.GLUT
import Data.IORef
import Data.Time
import Graphics.UI.GLUT.Raw.Functions -- }}}

-- * GameDefinition -- {{{
-- | GameCallback represents the type of a standard \"callback\" 
-- This is used for the type of callbacks in 'GameDefinition' 
type GameCallback a = a -> IO a

-- docs -- {{{
{- |
  This acts as a complete definition for the application,
  acting as sole parameter for 'runGame', the top-level method
  of this module.

  It takes as type parameter the type of the \"game state\"
  to be used to hold the state of the game throughout the
  course of the program's operation 
-} -- }}}
data GameDefinition a = 
  GameDefinition -- {{{
    {
    -- settings
      -- | defines the window title 
      -- (takes as parameter program name and the program arguments)
      gameWindowName :: (String, [String]) -> IO String
      -- | defines the window size GLUT tries to make the window
      -- (takes as parameter program name and the program arguments)
    , gameInitialWindowSize :: (String, [String]) -> IO (Int, Int)
      -- | defines the initial game state
      -- (takes as parameter program name and the program arguments)
    , gameInitial :: (String, [String]) -> IO a
      -- | this is the color that will be used to
      -- clear the color buffer
      -- (takes as parameter program name and the program arguments)
    , gameBackground :: (String, [String]) -> IO DColor4
      -- | defines the framerate at which 'gameDisplay'
      -- 'gameIdle' will be activated 
      -- (takes as parameter program name and the program arguments)
    , gameFramerate :: (String, [String]) -> IO Int
    -- callbacks
      -- | this is called when: the window has not been refreshed 
      -- for 1/framerate seconds or GLUT decides to call it for 
      -- other various reasons
    , gameDisplay :: GameCallback a
      -- | this is called when the window is reshaped (takes as
      -- parameter the new windows dimensions in pixels)
    , gameReshape :: (Int, Int) -> GameCallback a
      -- | this is called when GLUT catches
      -- some event (takes as parameter the event caught)
    , gameEvent :: GameEvent -> GameCallback a
      -- | this is called when it hasn't been called
      -- for over 1/gameFramerate seconds
    , gameIdle :: Double -> GameCallback a }  -- }}}

-- | supplied an initial game state, this game definition
-- acts as an "empty" program, suitable as a base 
-- to start with 
gameDefinition :: a -> GameDefinition a 
gameDefinition a = -- {{{
  GameDefinition
    { 
    -- settings
      gameWindowName = const . return $ "PixelGame"
    , gameInitialWindowSize = const . return $ (500, 500)
    , gameInitial = const . return $ a
    , gameBackground = const . return $ colorBlack
    , gameFramerate = const . return $ 40
    -- callbacks
    , gameDisplay = 
        \x ->
          do
            clear [ ColorBuffer ]
            flush 
            return x 
    , gameReshape = const return 
    , gameEvent = const return 
    , gameIdle = const return }  -- }}}

-- docs -- {{{
{- |
  This acts as a unified representation of all events that
  the user could throw at the program.
  
  It uses some data types imported from "Graphics.UI.GLUT".
  
  Mouse coordinates are supplied with each dimension in the range
  [-1, 1], scaled linearly across the size of the window.
-} -- }}}
data GameEvent = 
  -- | called when the user presses something
  GameKeyEvent Key KeyState DVector
  -- | called when the user moves the mouse
  -- accepts a single parameter, the mouse coordinates resultant
  -- of the movement
  | GameMotionEvent DVector 
 -- }}}

-- * runGame  -- {{{
-- docs -- {{{
{- |
  This is the top-most method of this module; it runs a 
  'GameDefinition'. Not much to say, except that it does
  quite a bit of stuff to initialise, sets up the callbacks,
  and then passes the program off to GLUT to run the main
  program loop. 
-} -- }}}
runGame :: GameDefinition a -> IO () 
runGame def = do
  -- initialize -- {{{
  progArgs <- getArgsAndInitialize

  -- create window
  windowName <- gameWindowName def progArgs
  (windowSizeX, windowSizeY) <- gameInitialWindowSize def progArgs
  initialWindowSize $= Size (fromIntegral windowSizeX) (fromIntegral windowSizeY)
  createWindow windowName

  -- settings
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  lineWidth $= 2
  glutIgnoreKeyRepeat 1

  initialDisplayMode    $= [ DoubleBuffered ]

  background <- realToFracColor4 <$> gameBackground def progArgs
  clearColor $= background

  -- create IORefs
    -- for world
  initialWorld <- gameInitial def progArgs
  world <- newIORef initialWorld

    -- for the framerate limiter
  lastRef <- (getCurrentTime :: IO UTCTime) >>= newIORef
  deadRef <- newIORef (0 :: Double)

  -- clear the window, why not.
  clear [ ColorBuffer ]

  -- set callbacks
  displayCallback       $= myDisplayCallback def world
  reshapeCallback       $= (Just $ myReshapeCallback def world)
  keyboardMouseCallback $= (Just $ myKeyboardMouseCallback def world)
  myFramerate <- gameFramerate def progArgs
  motionCallback        $= Just (myMotionCallback def world)
  passiveMotionCallback $= Just (myMotionCallback def world)
  idleCallback          $= 
    Just (myIdleCallback def world (lastRef, deadRef) myFramerate)

  -- alright, off to mainLoop now!
  mainLoop  -- }}}

windowToReal :: Position -> IO DVector 
windowToReal (Position xp yp) = -- {{{
  do
    Size w h <- get windowSize 
    return $
      E.Vector
        (realToFrac . f $ 1 / fromIntegral w * fromIntegral xp)
        (realToFrac . f $ 1.0 - 1.0 / fromIntegral h * fromIntegral yp)
  where
    f = (*2) . flip (-) (0.5 :: Double) 
-- used to determine the mouse position
-- used in GameEvent values  -- }}}

runCallback :: IORef a -> GameCallback a -> IO () 
runCallback ref aCallback =
  writeIORef ref =<< aCallback =<< readIORef ref

myReshapeCallback  :: GameDefinition a -> IORef a -> ReshapeCallback 
myReshapeCallback def ref (Size xi yi) =
  let intSize = (fromIntegral xi, fromIntegral yi) 
  in
    do
      viewport $= (Position 0 0, Size xi yi)
      runCallback ref (gameReshape def intSize)  

myDisplayCallback  :: GameDefinition a -> IORef a -> DisplayCallback 
myDisplayCallback def ref = runCallback ref (gameDisplay def)  

myKeyboardMouseCallback  ::  GameDefinition a -> IORef a -> KeyboardMouseCallback 
myKeyboardMouseCallback def ref key keystate _ pos = 
  do
    event <- GameKeyEvent key keystate <$> windowToReal pos
    runCallback ref (gameEvent def event)   

myMotionCallback  :: GameDefinition a -> IORef a -> MotionCallback 
myMotionCallback def ref pos = 
  do
    event <- GameMotionEvent <$> windowToReal pos
    runCallback ref $ gameEvent def event  

myIdleCallback 
  :: GameDefinition a -> IORef a -> 
      (IORef UTCTime, IORef Double) -> Int -> IdleCallback
myIdleCallback def ref (lastRef, deadRef) fr =
  do
    nowTime <- getCurrentTime
    lastTime <- readIORef lastRef
    modifyIORef' deadRef (+ (realToFrac $ diffUTCTime nowTime lastTime))
    deadTime <- readIORef deadRef
    when (deadTime >= 1 / fromIntegral fr) 
      (do
        runCallback ref (gameIdle def deadTime)
        myDisplayCallback def ref
        writeIORef deadRef 0)
    writeIORef lastRef nowTime  
 -- }}}
