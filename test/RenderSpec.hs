{-# LANGUAGE FlexibleInstances #-}
module RenderSpec where
import GHC.Word (Word8)
import Test.Hspec
import Render (getPatImage, getImageFromPatterns)
import Codec.Picture (Image(..), PixelRGBA8, Pixel (..), generateImage)
import Patterns (initializePatternsFromImage, maxLoc, dims)
import Test.QuickCheck ((==>), choose, infiniteListOf, forAll, Arbitrary (..), vectorOf, forAllShrink, Positive (..), (===))
import Test.Hspec.QuickCheck (prop)
import qualified Data.Vector.Storable as VS
import qualified Data.Map.Strict as M
import Point (getCoords)
import Utils (mapTuple, subModn)
import Data.List (sort)

spec :: Spec
spec = describe "test render functions" $ do
  prop "image from parsed patterns == original image" $ do
    \ (Positive x) (Positive y) (Positive n) ->
      n < x && n < y ==>
      -- x > 3 && y > 3 ==>
        forAll (vectorOf (x*y*4) (choose (0,255))) $ \rs -> do
          let img = Image x y (VS.fromList rs)
          let ps = initializePatternsFromImage img n
          let odims = mapTuple (subModn n) (x,y)
          let img2 = getImageFromPatterns ps odims
          let sortedPixels = sort . VS.toList . imageData
          sortedPixels (trimGTn img n) === sortedPixels img2

trimGTn img n = 
  uncurry (generateImage (pixelAt img)) (mapTuple (subModn n) wh)
  where wh = dims img

imageToPntMap img = M.fromList [(pnt,uncurry (pixelAt img) pnt) | pnt <- getCoords (dims img)]
