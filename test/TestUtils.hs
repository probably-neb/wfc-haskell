module TestUtils where

import Codec.Picture
import Codec.Picture.Types
import Patterns
import Point
import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Vector.Storable as VS
import Test.QuickCheck
import Wfc (setupModel, Model)
import Patterns.Internal

genFakePixelCheckerBoard :: Int -> Int -> Int -> PixelRGBA8
genFakePixelCheckerBoard n x y | helper x == helper y = PixelRGBA8 255 255 255 255
                             | otherwise = PixelRGBA8 0 0 0 255
                          where helper d = even $ div d n

genFakeImage :: Int -> Image PixelRGBA8
genFakeImage n = generateImage (genFakePixelCheckerBoard n) 50 50

genImage :: Gen (Image PixelRGBA8)
genImage = do
  (x,y) <- genWH
  rs <- vectorOf (x*y*4) (choose (0,255)) :: Gen [Word8]
  return (Image x y (VS.fromList rs))

foldTrue :: Foldable f => f Bool -> Bool
foldTrue = and

getRs = do rs <- generate infiniteList :: IO [Int]
           return rs

getMod :: (Int,Int) -> IO Model
getMod wh = do rs <- getRs
               return (setupModel (rs) wh getSimplePatterns 4)

getM2 :: IO Model
getM2 = getMod (4,4)

genPoint :: Gen Point
genPoint = do
    w <- chooseInt (0,100)
    h <- chooseInt (0,100)
    return (w,h)

-- point with min 1
genWH :: Gen Point
genWH = do
    w <- chooseInt (1,10)
    h <- chooseInt (1,10)
    return (w,h)
