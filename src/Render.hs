{-# LANGUAGE FlexibleContexts #-}
module Render where

import Graphics.Gloss.Data.Display (Display (..))
import Graphics.Gloss (Picture)
import qualified Graphics.Gloss as GL

import qualified Data.Map.Strict as M

import Patterns
import Wfc
import Utils
import Point
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import Data.Word

-- TODO: make take image to enforce types

translateByPoint :: Point -- ^ grep
  -> Int -- ^ goob
  -> Picture -- ^ gob
  -> Picture
translateByPoint p n pic = GL.translate xf yf pic
  where (xf,yf) = mapTuple (fromIntegral.(*n)) p

maybeHead :: V.Vector a -> Maybe a
maybeHead v | V.null v = Nothing
            | otherwise = Just (V.head v)

modelToPicture :: Point -> (Float,Float) -> Model -> Picture
modelToPicture trans
               (scaleX,scaleY)
               m@Model{m'step=mstep,m'n=n,m'outDims=odims}
  = GL.Pictures
    $ (\p -> [countPic,p])
    $ translateByPoint trans 1
    $ GL.scale scaleX scaleY
    $ GL.Pictures
    $ map (\t -> translateByPoint (t'loc t) n (t'pic t))
   (m'tiles m)
     where maxPnt = fst $ M.findMax (m'board m)
           doneClr = GL.color $ if m'done m
                                  then GL.green
                                  else GL.white
           countNumPic = translateByPoint (-1,-1) n $ GL.Text $ show mstep
           countPic = if m'done m
                      then 
                        GL.pictures [GL.color GL.green $ GL.circle 10.0, countNumPic]
                      else 
                        countNumPic

test :: IO ()
test = do
  ps <- Patterns.test
  let pic = patternsToPicture ps
  print $ length ps
  print $ map p'vec ps
  let displayMode = FullScreen
  GL.display displayMode GL.white pic

displayPic :: Picture -> IO ()
displayPic pic = do
  let displayMode = FullScreen
  GL.display displayMode GL.white pic
