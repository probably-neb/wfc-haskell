{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}
module Main where

-- import System.Directory
import Entropy
import Patterns
import Patterns.Internal (getSimplePatterns)
import Codec.Picture
import Data.List
import Utils ( WdHt, mapTuple )
import Point
import qualified Data.Set as Set
import Wfc
import qualified Render
-- import Codec.Picture.Types

-- TODO: REMOVE
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Display (Display (..))
import Graphics.Gloss (Picture)
import qualified Graphics.Gloss as GL
import Graphics.Gloss.Interface.IO.Game (Event (..), SpecialKey (..), KeyState (..), Key (..))
import Control.Monad.State (execState)
import Data.Time
import System.Random (mkStdGen, randoms)

-- VARIABLES --
n :: Int
n = 4 -- pattern side-length
outputDims :: (Int,Int)
outputDims = (100,100)
outputVisDims :: (Int, Int)
outputVisDims = (1000,1000)
outputBoarderWidth :: Int
outputBoarderWidth = 25
-- VARIABLES --
hz = 10

circuitPath :: (FilePath, Int)
circuitPath = ("inputs/circuit.png", 20)
checkerPath :: (FilePath, Int)
checkerPath = ("inputs/checkerboard.png",3)
mazePath :: (FilePath, Int)
mazePath = ("inputs/Mazelike.png", 4)
celticPath :: (FilePath, Int)
celticPath = ("inputs/celtic.png",10)

outputWdHt :: WdHt
outputWdHt = outputDims

outputTileDims :: WdHt
outputTileDims = mapTuple ((`div` n) . (+(n-1))) outputDims

outputTranslate :: (Int, Int)
outputTranslate = (negate x, negate y)
  where (hboardx,hboardy) = mapTuple (`div` 2) outputVisDims
        (x,y) = (hboardx - outputBoarderWidth, hboardy - outputBoarderWidth)

outputScale :: (Float, Float)
outputScale = (sx,sy)
  where (winxF,winyF) = mapTuple fromIntegral outputVisDims :: (Float,Float)
        (outxF,outyF) = mapTuple fromIntegral outputDims :: (Float,Float)
        boarddF = fromIntegral (2* outputBoarderWidth)
        sx = (winxF - boarddF) / outxF
        sy = (winyF - boarddF) / outyF

main :: IO ()
main = do
  let (path,rn) = mazePath
  img <- loadInput path
  let lplst = getPatternsFromImage img rn
  rands <- getRandoms outputTileDims
  let rn = 4
  let plst = getSimplePatterns
  let
      initModel = setupModel rands outputTileDims plst rn
  playInteractiveWfc initModel

playWfc :: Model -> IO ()
playWfc initModel = 
  GL.simulate
    (GL.InWindow "WAVE FUNCTION COLLAPSE" outputVisDims (0,0))
    GL.white
    hz
    initModel
    (Render.modelToPicture outputTranslate outputScale) Wfc.stepM

playInteractiveWfc :: Model -> IO ()
playInteractiveWfc initModel = 
  GL.play
    (GL.InWindow "WAVE FUNCTION COLLAPSE" outputVisDims (0,0)) 
    GL.white 
    0 -- don't step based on time
    initModel
    (Render.modelToPicture outputTranslate outputScale)
    inputHandler
    (flip const) -- don't step based on time

inputHandler :: Event -> Model -> Model
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) m= 
  execState observeNextM m
inputHandler (EventKey (Char 'c') Down _ _) m = 
  execState observeUntilCollapse m
-- inputHandleer (EventKey (MouseButton LeftButton) Down x y) = 
inputHandler _ m = m

translateByPoint :: Point -> Picture -> Picture
translateByPoint p pic = GL.translate xf yf pic
  where (xf,yf) = mapTuple fromIntegral p

getRandoms :: WdHt -> IO [Int]
getRandoms (w,h) = do currTime <- getCurrentTime :: IO UTCTime
                      let time = floor $ utctDayTime currTime :: Int
                      let gen = mkStdGen time 
                      let infRands = randoms gen :: [Int]
                      return infRands

showPatterns :: IO ()
showPatterns = do
  let (path,rn) = circuitPath
  img <-loadInput path
  let plst = getPatternsFromImage img rn
  let pic = patternsToPicture plst
  GL.display FullScreen GL.white pic

-- | Run a finite-time-step simulation in a window. You decide how the model is represented,
--      how to convert the model to a picture, and how to advance the model for each unit of time.
--      This function does the rest.
--
--   Once the window is open you can use the same commands as with `display`.
--
-- simulate
--         :: Display      -- ^ Display mode.
--         -> Color        -- ^ Background color.
--         -> Int          -- ^ Number of simulation steps to take for each second of real time.
--         -> model        -- ^ The initial model.
--         -> (model -> Picture)
--                 -- ^ A function to convert the model to a picture.
--         -> (ViewPort -> Float -> model -> model)
--                 -- ^ A function to step the model one iteration. It is passed the
--                 --     current viewport and the amount of time for this simulation
--                 --     step (in seconds).
--         -> IO ()
