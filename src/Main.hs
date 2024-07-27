{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}
module Main where

import Patterns
import Codec.Picture
import Utils ( WdHt, mapTuple )
import Point
import Wfc
import qualified Render

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
hz = 20

circuitPath :: (FilePath, Int)
circuitPath = ("inputs/circuit.png", 32)
checkerPath :: (FilePath, Int)
checkerPath = ("inputs/checkerboard.png",3)
mazePath :: (FilePath, Int)
mazePath = ("inputs/Mazelike.png", 2)
celticPath :: (FilePath, Int)
celticPath = ("inputs/celtic.png",32)

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
  let (path,rn) = celticPath
  img <- loadInput path
  let plst = getPatternsFromImage img rn
  rands <- getRandoms outputTileDims
  -- let rn = 4
  -- let plst = getSimplePatterns
  let initModel = setupModel rands outputTileDims plst rn
  playWfc initModel
  -- runWfc initModel

wfc :: Image PixelRGBA8 -> WdHt -> Int -> Int -> Image PixelRGBA8
wfc img wdht n seed = output
  where plst = getPatternsFromImage img n
        gen = mkStdGen seed
        rands = randoms gen
        mi = setupModel rands wdht plst n
        mf = last $ stepUntilDone mi
        output = Render.getModelImage mf

runWfc :: Model -> IO ()
runWfc m = do Render.storeModelImage end filename
  where end = last (stepUntilDone m)
        rand = sum $ map t'rand $ m'tiles end
        filename = "./outputs/wfc-" ++ show rand ++ ".png"

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
  step m
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

