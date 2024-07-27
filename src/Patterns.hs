{-# LANGUAGE FlexibleInstances #-}
module Patterns where

import GHC.Stack
import Debug.Trace
import Codec.Picture
-- import Codec.Picture.Types
-- import Data.Map.Lazy (Map)
import Data.List.Split (chunksOf)
import Data.Map ((!), Map)
import qualified Data.Map as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
-- import Codec.Picture.Types
import qualified Data.Vector as V
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.List (group,sort,nub)
import Data.Maybe
import Data.Hashable
import Data.Word
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Merge.Strict as MM
import Data.IntMap.Merge.Strict as MI

import Point (Point)
import qualified Point as P
import Utils

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Display (Display (..))
import Graphics.Gloss (Picture)
import qualified Graphics.Gloss as GL


type IdVec = V.Vector Double -- <{0.0|1.0}...>
ivB :: Double -> Bool
ivB d | d == 1.0 = True
      | d == 0.0 = False
      | otherwise = error "Non Identity Vector Value"


falseIdVec :: Int -> IdVec
falseIdVec len = V.replicate len 0.0

trueIdVec :: Int -> IdVec
trueIdVec len = V.replicate len 1.0

(&!) = andIdVecs

andIdVecs :: IdVec -> IdVec -> IdVec
andIdVecs = V.zipWith (*)

(|!) = orIdVecs

orIdVecs :: IdVec -> IdVec -> IdVec
orIdVecs v1 v2 = V.zipWith max v1 v2

orIdVecsf :: Foldable f => f IdVec -> IdVec
orIdVecsf vs = foldr1 orIdVecs vs

andNIdVec :: HasCallStack => IdVec -> V.Vector a -> V.Vector (Maybe a)
andNIdVec = V.zipWith (\idv a -> if idv == 1.0 then Just a else Nothing)

isTrue' :: (Show a) => Double -> a -> Bool
isTrue' b a | b == 1.0 = True
            | b == 0.0 = False
            | otherwise = error ("Non id value in IdVec: " ++ (show a))

extractTrue :: Show a => IdVec -> V.Vector a -> V.Vector a
extractTrue iv va = (V.ifilter (\i _ -> (iv V.! i) == 1.0) va)

mapTrue :: (Show b) => (a -> b) -> (a -> b) -> IdVec -> V.Vector a -> V.Vector b
mapTrue tf ff iv va = (V.imap (\i ai -> if (iv V.! i) == 1.0 then tf ai else ff ai) va)

type AdjacentsMap = Map Point IdVec

defaultAdjMap :: Int -> AdjacentsMap
defaultAdjMap len = (M.fromList $ zip P.cardinalDirs (repeat emptyv))
        where emptyv = falseIdVec len :: IdVec -- for type checker

------------------------------------------------------------------------------------------
data Pattern = Pattern {
  p'pic :: Picture,
  p'vec :: Vector Word8,
  p'loc :: Point, -- top left corner == 0,0 x+
  p'rc :: RwCl, -- rows and cols of original image
  p'n :: Int,
  p'id :: Int,
  p'hash :: Hash,
  p'adjacents :: AdjacentsMap,
  -- the probability of this pattern (freq of this pattern / total num patterns)
  p'prob :: Double -- ???: freq pattern / total patterns == pattern (1) / total UNIQUE patterns
}

emptyPattern :: Pattern
emptyPattern = Pattern {p'pic = GL.blank, p'vec = VS.empty, p'loc = (-1,-1), p'rc= (0,0), p'n=0, p'hash=0, p'prob=0.0, p'adjacents = M.empty, p'id = -1}

hashPatternVec :: (Show a, VS.Storable a) => Vector a -> Hash
hashPatternVec vec = hash $ show vec

instance Show Pattern where
  show pat = concat ["Pattern(ID:",show (p'id pat),", ",(show (p'loc pat)),",",show (p'prob pat * 100.0),"%,", show (p'hash pat),")"]

instance Eq Pattern where
  x == y | p'hash x > 0 && p'hash y > 0               = p'hash x == p'hash y
         | p'vec x /= VS.empty && p'vec y /= VS.empty   = p'vec x == p'vec y
         | otherwise                                    = p'loc x == p'loc y -- last resort hope it never comes to this

instance Ord Pattern where
  compare a b | p'prob a /= 0 && p'prob b /= 0 && p'prob b /= p'prob a = compare (p'prob a) (p'prob b) -- flip to make max prob come before lower prob
              | otherwise = compare (p'loc a) (p'loc b)

instance Hashable Pattern where
  -- NOTE: patterns are matched using hashes so rotations may need to use them
  hash p = hashPatternVec $ p'vec p
  hashWithSalt salt p = hashWithSalt salt $ hash p

instance Show (Image PixelRGBA8) where
  show img = concat ["Image: (",show $ imageWidth img,",",show $ imageHeight img, ") Vec: ", show $ imageData img]

--------------------------------------------------------------------------------------------------------

-- make2d :: V.Storable a => V.Vector a -> V.Vector a
-- make2d arr = chunksOf n $ chunksOf 3 arr
-- rotate :: [[a]] -> [[a]]
-- rotateRight = transpose . reverse
-- rotateLeft = reverse . transpose

vectorToPicture :: Vector Word8 -> WdHt -> Picture
vectorToPicture v (w, h) = bitmapOfForeignPtr w h bitmapFormat foreignPtr True
  where
        pixelFormat = PxRGBA
        rowOrder = TopToBottom
        bitmapFormat = BitmapFormat TopToBottom PxRGBA
        (foreignPtr, len) = VS.unsafeToForeignPtr0 v

patternToPicture :: Pattern -> Picture
patternToPicture p = GL.Translate x y (vectorToPicture (p'vec p) (n,n))
  where n = p'n p
        (x,y) = mapTuple fromIntegral (p'loc p) :: (Float, Float)

patternsToPicture :: [Pattern] -> Picture

patternsToPicture ps = GL.Pictures $ map patternToPicture ps

dims :: Image PixelRGBA8 -> WdHt
dims img = (imageWidth img, imageHeight img)

saveImg :: PngSavable a => FilePath -> Image a -> IO ()
saveImg fp img = do
    writePng fp img

-- saveFakeImage :: IO ()
-- saveFakeImage = 
--   saveImg "outputs/fakeimage.png" img1
--   where 
--     img1 = genFakeImage 500

loadInput :: FilePath -> IO (Image PixelRGBA8)
loadInput fp = do
  img <- readImage fp
  return (
    case img of
       Left s -> error "Failed to load input"
       Right i -> convertRGBA8 i
   )

dirToIdx :: Point -> Int
dirToIdx rc | rc == (-1,0) = 0
            | rc == (0,-1) = 1
            | rc == (0,1) = 2
            | rc == (1,0) = 3
            | otherwise = -1
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

intMapPatternWith :: HasCallStack => [Pattern] -> (Pattern -> Int) -> (Pattern -> a) -> (a -> a -> a) -> IntMap a
intMapPatternWith ps kf vf combf = I.fromListWith combf [(kf p, vf p) | p <- ps]

intMapPattern :: HasCallStack => [Pattern] -> (Pattern -> Int) -> (Pattern -> a) -> IntMap a
intMapPattern ps kf vf = intMapPatternWith ps kf vf const

mapPattern :: HasCallStack => (Ord a) => [Pattern] -> (Pattern -> a) -> (Pattern -> b) -> Map a b
mapPattern ps kf vf = M.fromList [(kf p, vf p) | p <- ps]

getFreqMap :: HasCallStack => [Pattern] -> IntMap Double
getFreqMap ps = I.fromListWith (+) [(p'hash p, 1.0) | p <- ps]

getDirAdjacentPatterns :: HasCallStack => Pattern -> [(Point,Hash)]
getDirAdjacentPatterns p = undefined

adjacentsPat :: HasCallStack => Pattern -> [Point]
adjacentsPat p = P.unsafeAdjacents (p'n p) (p'loc p)

-- essentially skip lvl1 and return merge of lvl2
mergeMaps :: HasCallStack => Map Point (IdVec) -> Map Point (IdVec) -> Map Point (IdVec)
mergeMaps ma mb = MM.merge MM.preserveMissing' MM.preserveMissing' (MM.zipWithMatched (ignoreFirstArg orIdVecs)) ma mb

mergePatterns :: HasCallStack => Pattern -> Pattern -> Pattern
mergePatterns (p1@(Pattern {p'adjacents=p1adj})) (p2@(Pattern {p'adjacents=p2adj})) = p1 {p'adjacents = mergeMaps p1adj p2adj}

getHashPatternMap :: HasCallStack => [Pattern] -> IntMap Pattern
getHashPatternMap ps = I.fromListWith mergePatterns [(p'hash p, p) | p <- ps]

mergeDuplicatePatterns :: HasCallStack => [Pattern] -> [Pattern]
mergeDuplicatePatterns ps = I.elems hashPatternMap
  where hashPatternMap = intMapPatternWith ps p'hash id mergePatterns :: IntMap Pattern

addPicturesToPatterns :: HasCallStack => [Pattern] -> [Pattern]
addPicturesToPatterns ps = [p{p'pic = vectorToPicture (p'vec p) (p'n p, p'n p)} | p <- ps]

getDirMaybePoints :: Map Point Int -> [(Point,Point)] -> [(Point, Maybe Int)]
getDirMaybePoints pidMap dirAdjPoints = map (\(a,b) -> (a, pidMap M.!? b)) dirAdjPoints

-- extracted for clarity
addNeighborsToPattern :: HasCallStack => Map Point Int -> IdVec -> Pattern -> Pattern
addNeighborsToPattern pidMap idVec p@(Pattern {p'adjacents=adjs, p'loc=ploc}) = p {p'adjacents = updatedAdjs}
  where dirAdjPoints = [(P.getDirection npnt ploc,npnt) | npnt <- adjacentsPat p]
        dirMaybePoints = getDirMaybePoints pidMap dirAdjPoints
        dirInboundsPointIdVecs = map (\(a,b) -> case b of
                                                Just i ->
                                                  (a,idVec V.// [(i,1.0)])
                                                Nothing ->
                                                  (a,idVec))
                                            dirMaybePoints
        updatedAdjs = foldr (\(dir,adjVec) mp' -> M.insert dir adjVec mp') adjs dirInboundsPointIdVecs

-- filters out repeating patterns and adds neighbors to unique patterns
addNeighborsToPatterns :: HasCallStack => ([Pattern], IdVec) -> [Pattern]
addNeighborsToPatterns (ps,idVec) = map (addNeighborsToPattern pidMap idVec) ps
  where pidMap = mapPattern ps p'loc p'id :: Map Point Int
        -- numUniquePatterns = foldr max 0 (M.elems pidMap)
        -- idVec = falseIdVec numUniquePatterns

addProbabilitiesToPatterns :: HasCallStack => [Pattern] -> [Pattern]
addProbabilitiesToPatterns ps = [p{p'prob = ((freqMap I.! (p'hash p)) / total)} | p <- ps]
  where freqMap = intMapPatternWith ps p'hash (const 1.0) (+) -- {Hash:Frequency}
        total = fromIntegral $ length ps :: Double

addIdsAndDefaultAdjMapToPatterns :: HasCallStack => [Pattern] -> ([Pattern],IdVec)
addIdsAndDefaultAdjMapToPatterns ps = (newPs,falseIdVec maxId)
  where
    incrementId :: Int -> Int -> (Int, Int)
    incrementId a b = (a + 1, b + a)
    hash0Map = mapPattern ps p'hash (const 0) -- {pattern : 0}
    (maxId,hashIdMap) = M.mapAccum incrementId 0 hash0Map
    addId :: Pattern -> Int -> Pattern
    addId p id' = p{p'id=id',p'adjacents=defaultAdjMap maxId}
    newPs = map (\p -> addId p (hashIdMap M.! (p'hash p))) ps

addIdsToPatterns :: [Pattern] -> [Pattern]
addIdsToPatterns ps = iddPatterns
  where hashSet = S.fromList $ map p'hash ps
        iddPatterns = map (\p@Pattern{p'hash=hsh} -> p{p'id=S.findIndex hsh hashSet}) ps

-- addHashesToPatterns :: [Pattern] -> [Pattern]
-- addHashesToPatterns ps = [p{p'hash = hash p} | p <- ps]

-- this is to be the only funtion that returns a list of patterns from something besides a list of patterns so it has to be first when the functions are composed
initializePatternsFromImage :: Image PixelRGBA8 -> Int -> [Pattern]
initializePatternsFromImage img n =
     [emptyPattern {
      p'pic=GL.blank,
      p'vec=vec,
      p'loc=(xof,yof),
      p'rc=rowCol,
      p'n=n,
      p'hash = hashPatternVec vec}
        | (xof,yof) <- coords,
          let pImg = generateImage (\x y -> pixelAt img (x + xof) (y + yof)) n n,
          let vec = imageData pImg
     ]

  where
    dimens = dims img
    rowCol = P.getRwCls dimens n
    coords = P.getCoords $ mapTuple (subModn n) dimens
    -- coords = P.getPatCoords rowCol n -- coordinates of top left corner of each pattern

getPatternsFromImage :: HasCallStack => Image PixelRGBA8 -> Int -> [Pattern]
getPatternsFromImage img n = addPicturesToPatterns -- done last to avoid repeat conversions
                             . mergeDuplicatePatterns
                             . addNeighborsToPatterns -- before merge so that adjacents get merged
                             . addIdsAndDefaultAdjMapToPatterns -- must be done before neighbors
                             . addProbabilitiesToPatterns
                             $ initializePatternsFromImage img n

maxLoc :: [Pattern] -> Point
maxLoc ps = maximum $ map p'loc ps
