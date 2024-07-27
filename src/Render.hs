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
import Codec.Picture (PixelRGBA8 (..), Image (..), generateImage, writePng, imageData)
import Data.List (sort, sortOn)

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

patternGrid :: V.Vector Pattern -> Point -> Int -> [Picture]
patternGrid ps (w,h) n = ppics
  where plst = V.toList ps
        ppics = [translateByPoint (w,h+2*(p'id p)) n (p'pic p) | p <- plst]

modelToPicture :: Point -> (Float,Float) -> Model -> Picture
modelToPicture trans
               (scaleX,scaleY)
               m@Model{m'step=mstep,m'n=n,m'outDims=odims}
  = GL.Pictures
    $ (\p -> [countPic,p])
    $ translateByPoint trans 1
    $ GL.scale scaleX scaleY
    $ GL.Pictures
    $ (patternGrid (m'patterns m) odims n) ++ (map (\t -> translateByPoint (t'loc t) n (t'pic t)))
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


idxImgVec :: Integral a => a -> a -> a -> a
idxImgVec n x y = (mod y n * n + mod x n)*4

pixelFromIdx :: VS.Vector Word8 -> Int -> PixelRGBA8
pixelFromIdx v idx = PixelRGBA8 r g b a
  where vslice = VS.slice idx 4 v
        [r,g,b,a] = VS.toList vslice

picsWithLocsToPicture :: [(Point,VS.Vector Word8)] -> Point -> Int -> Image PixelRGBA8
picsWithLocsToPicture pps (w,h) n =
  generateImage (\ x y ->
                    let imgVec = pointMap M.! (div x n,div y n) in
                        pixelFromIdx imgVec (idxImgVec n x y)
                ) (w*n) (h*n)

  where pointMap = M.fromList pps

getPatImage :: [Pattern] -> Tile -> VS.Vector Word8
getPatImage ps t = case V.elemIndex 1.0 (t'domain t) of
                     Just idx -> p'vec (ps !! idx)
                     Nothing -> deflt
    where deflt = VS.map (const 0) $ p'vec $ head ps

mapModelToPointPics :: Model -> [(Point,VS.Vector Word8)]
mapModelToPointPics m = tozip t'loc (getPatImage $ V.toList pats) (m'tiles m)
  where pats = m'patterns m

getModelImage m = pic
  where pntPics = mapModelToPointPics m
        pic = picsWithLocsToPicture pntPics (m'outDims m) (m'n m)

storeModelImage :: Model -> FilePath -> IO ()
storeModelImage m fp = do let pic = getModelImage m
                          writePng fp pic   

getImageFromPatterns :: [Pattern] -> WdHt -> Image PixelRGBA8
getImageFromPatterns ps (w,h) = Image w h imgvec
  where n = p'n $ head ps
        ivs = map p'vec $ sortOn p'loc $ filter (((0,0) == ) . mapTuple (`mod` n) . p'loc) ps
        imgvec = foldl (VS.++) VS.empty ivs

-- getImageFromPatterns :: [Pattern] -> WdHt -> Image PixelRGBA8
-- getImageFromPatterns ps wh = pic
--   where n = p'n $ head ps
--         pntpics = tozip p'loc p'vec $ filter (((0,0) == ) . mapTuple (`mod` n) . p'loc) ps
--         pic = picsWithLocsToPicture pntpics wh 1

storePatternsImage :: [Pattern] -> FilePath -> IO ()
storePatternsImage ps fp =
  do let pic = getImageFromPatterns ps (maxLoc ps)
     writePng fp pic
     displayPic $ vectorToPicture (imageData pic) (imageWidth pic, imageHeight pic)

displayPic :: Picture -> IO ()
displayPic pic = do
  let displayMode = FullScreen
  GL.display displayMode GL.white pic
