module WfcSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Modifiers (NonEmptyList(..),InfiniteList)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (group,sort,mapAccumL,unfoldr, nub)
import qualified Data.List as L
import Wfc
import Patterns
import Patterns.Internal
import Stack (pop,push,pushl)
import qualified Stack
import Utils
import TestUtils
import Point

import Control.Monad.Trans.State
import Data.Foldable (find)
import Test.QuickCheck
import Data.Maybe (isJust, fromMaybe, mapMaybe, isNothing, fromJust, catMaybes)
import Render
import Data.Word (Word8)
import qualified Data.Vector.Generic as VG
import Data.Function ((&))
import qualified Control.Arrow as Bi

simplePatterns = getSimplePatterns

n = 4
dims' = (10,10)
spec :: Spec
spec =  describe "Wfc" $ do
    rands <- runIO getRands
    let m = setupModel (head rands) dims' simplePatterns n
    prop "every tiles domain should be an intersection of its surroundings" $
      forAllShrink genSteppedModel shrink $ \mod -> do
        allTilesInBoardIntersectionOfAdjs mod == True
            -- foldTrue bools `shouldBe` True
            -- mapM (shouldBe True) bools
    prop "stepping a model should result in done, collapse tile, or changed stack" $ do
      forAllShrink genSteppedModel shrink $ \modi -> 
        (not $ m'done modi) ==> do
          let stacki = m'stack modi
              ncolpsi = length $ filter t'collapsed $ m'tiles modi
              modf = step modi
              stackf = m'stack modf
              donef = m'done modf
              ncolpsf = length $ filter t'collapsed $ m'tiles modf
          (stacki /= stackf) || (ncolpsi /= ncolpsf) `shouldBe` True

    describe "finding min entropy" $ do
      context "with non empty list" $ do
        prop "isJust" $ do
          \(NonEmpty es) ->
            isJust (findMinEntropy [Tile{t'entropy = e,t'collapsed=False} | e <- es]) `shouldBe` True
        prop "/=collapsed_val" $ do
          \(NonEmpty es) ->
            case (findMinEntropy [Tile{t'entropy = e,t'collapsed=False} | e <- es ++ (replicate 10 collapsed_val)]) of
              Just minEntT -> do t'entropy minEntT `shouldNotBe` collapsed_val
              Nothing -> do (isJust Nothing) `shouldBe` True
        it "all collapsed_val should be Nothing" $ do
          let tiles = replicate 100 Tile{t'entropy=collapsed_val,t'collapsed=False}
          isJust(findMinEntropy tiles) `shouldBe` False
        it "all collapsed should be nothing" $ do
          let tiles =
                replicate 100 Tile{t'entropy=collapsed_val,t'collapsed=True}
          isJust(findMinEntropy tiles) `shouldBe` False
        prop "some are collapsed" $ do
          \(NonEmpty es) bl -> let tiles = [Tile{t'entropy= e,t'collapsed=bl} | e <- es] in
            case
              findMinEntropy tiles
            of
              Just minEnt -> t'collapsed minEnt `shouldBe` False
              Nothing -> -- check there was no other valid options
                length (filter (\t -> (t'entropy t /= collapsed_val)
                                       && not (t'collapsed t)) tiles)
                                 `shouldBe` 0
      it "empty list " $ do
        findMinEntropy [] `shouldBe` Nothing

    describe "model state stack operations" $ do
      it "pushing adds values" $ do
        let pushm = execState (stackPush push (0,0)) m
        m'stack pushm `shouldNotSatisfy` Stack.null
      prop "pushing multiple adds values" $ do
        \ is ->
              let
                ps = map dup is :: [Point]
                pushm = execState (stackPush pushl ps) m :: Model
              in
              length (Stack.asList $ m'stack pushm) `shouldBe` length ps

    describe "setupModel" $ do
        -- data Model = Model {
        --   m'outDims :: WdHt,
        --   m'board :: Map Point Tile,
        --   m'patterns :: Vector Pattern,
        --   m'probVec :: IdVec,
        --   m'adjVec :: Vector (Vector Double),
        --   m'n :: Int,
        --   m'len :: Int,
        --   m'done :: Bool
        -- }
      it "done should be false" $ do
        m'done m `shouldBe` False
      it "n should be n" $ do
        m'n m `shouldBe` n
      it "outDims `shouldBe` dims`" $ do
        m'outDims m `shouldBe` dims'
      let vlen = m'len m
      describe "adjVec" $ do
        it "adjVec should be of len 4*m'len" $ do
          V.length (m'adjVec m) `shouldBe` 4*vlen
        it "length of each elem of adjVec should be m'len" $ do
          let lens = V.toList $ V.map V.length $ m'adjVec m
          replicate (length lens) vlen `shouldBe` lens
        it "patterns directional domains should match adjVec" $ do
          let ps = V.toList $ m'patterns m
          let idPs = [(p'id p, M.assocs $ p'adjacents p) | p <- ps]
          let idAdjs = concatMap (\(pid,adjs)
                  -> [(pid,adj)|adj<-adjs]) idPs
          let idxPs = map (\(pid,(dr,aVec))
                  ->(indexAdjVec vlen dr pid,aVec)) idAdjs
          let bps = map (\(idx,aVec)
                  -> m'adjVec m V.! idx == aVec) idxPs
          foldTrue bps `shouldBe` True
      describe "probVec" $ do
        it "probVec should be of len m'len" $ do
          V.length (m'probVec m) `shouldBe` m'len m
        it "all probs should be 1.0 > p > 0.0" $ do
          let proobs = V.toList $ V.map (\p -> 1.0 > p && p > 0.0) (m'probVec m)
          foldTrue proobs `shouldBe` True
      describe "board" $ do
        let tiles = m'tiles m
        it "no tiles should be collapsed" $ do
          length (filter t'collapsed tiles) `shouldBe` 0
        it "all tiles entropy is equal" $ do
          let e = t'entropy $ head tiles
          let ents = map t'entropy tiles
          foldTrue (map (==e) ents) `shouldBe` True
        it "no tiles have entropy=collapsed_val" $ do
          let ents = map t'entropy tiles
          foldTrue (map (/=collapsed_val) ents) `shouldBe` True
      it "stack is empty" $ do
        m'stack m `shouldSatisfy` Stack.null

    -- let cm1 = execState m
    let (Just ctloc,cm) = runState collapseMinEntropyTile m
    describe "propogation" $ do
      let cmEmptyStack = cm{m'stack=Stack.empty}
      let (emptdone,emptCm) = runState recursivePropogate cmEmptyStack
      it "rec propogate on empty stack `shouldBe` done" $ do
        emptdone `shouldBe` True

    describe "pre first step checks" $ do
      it "before step there should only be one domain" $ do
        let grps = nub $ map t'domain $ m'tiles m
        length grps `shouldBe` 1
      it "before step there should only be one entropy" $ do
        let grps = nub $ map t'entropy $ m'tiles m
        length grps `shouldBe` 1
      it "before step there should not be any collapsed tiles" $ do
        let grps = filter t'collapsed $ m'tiles m
        length grps `shouldBe` 0

    -- describe "complete wfc" $ do
    --   let ms = stepUntilDone m
    --   let fin = last ms
    --   it "steps should equal number of tiles" $ do
    --     m'step fin `shouldBe` length (m'tiles fin)
      -- it "display pic" $ do
      --   let mi@Model{m'board=board} = last ms
      --   let newboard = M.map (\t -> t{t'pic=p'pic $ simplePatterns !! 4}) board :: M.Map Point Tile
      --   let pic = modelToPicture (0,0) (100.0,100.0) mi
      --   displayPic pic

xor a b = a /= b

getRands = sample' infiniteList :: IO [[Int]]

inverseDir = mapTuple ((-1) *)

getEitherTile :: Model -> Point -> IdVec -> Point -> IdVec
getEitherTile m tloc tdom dir =
  case m'board m M.!? add tloc dir of
    Just t -> let rps = extractTrue (t'domain t) (m'patterns m) in
                  domainInDirection rps (inverseDir dir) (m'len m)
    Nothing -> tdom
  where vlen = m'len m

allTilesInBoardIntersectionOfAdjs :: Model -> Bool
allTilesInBoardIntersectionOfAdjs mod = foldTrue bools
  where tileLocs = map t'loc $ m'tiles mod
        bools = map (`tdomIsIntersectionOfAdjacents` mod) tileLocs

tdomIsIntersectionOfAdjacents :: Point -> Model -> Bool
tdomIsIntersectionOfAdjacents tloc m = tdomS `S.isSubsetOf` surroundingDomainsIntersection
  where idxs v = S.fromList . V.toList $ V.elemIndices 1.0 v
        tdom = t'domain $ m'board m M.! tloc
        tdomS = idxs tdom
        tVec = idxs $ trueIdVec (m'len m) :: Set Int
        surroundingDomains' = map (idxs . (getEitherTile m tloc tdom)) cardinalDirs
        surroundingDomainsIntersection = foldl S.intersection tVec surroundingDomains'

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

removeSideFilter (x,y) t@Tile{t'loc=(tx,ty)} | tx == x = False
                                             | ty == y = False
                                             | otherwise = True

removeLowSideFilter t@Tile{t'loc=(x,y)} | x == 0 = False
                                        | y == 0 = False
                                        | otherwise = True

removeHighSideFilter (mx,my) t@Tile{t'loc=(x,y)} | x == mx = False
                                                 | y == my = False
                                                 | otherwise = True

addToLoc (ox,oy) t@Tile{t'loc=(tx,ty)} = t{t'loc=(newx,newy)}
  where newx = max 0 ox+tx
        newy = max 0 oy+ty

decrementLoc t@Tile{t'loc=(x,y)} = t{t'loc=(x-1,y-1)}

removeLowSide :: Model -> Model
removeLowSide m@Model{m'outDims=odims} = m{m'board=locFixedTilesMap,
                m'outDims=mapTuple (+ (-1)) odims}
  where tiles = map decrementLoc $ filter removeLowSideFilter (m'tiles m)
        locFixedTilesMap = M.fromList $ tozip t'loc id tiles

removeHighSide :: Model -> Model
removeHighSide m@Model{m'outDims=odims} = m{m'board=locFixedTilesMap,
                m'outDims=maxDims}
  where maxDims = mapTuple (+ (-1)) odims
        tiles = filter (removeHighSideFilter maxDims) (m'tiles m)
        locFixedTilesMap = M.fromList $ tozip t'loc id tiles

removeNorthSide :: Model -> Model
removeNorthSide m@Model{m'outDims=odims} = m{m'board=locFixedTilesMap,
                m'outDims=mapTuple (+ (-1)) odims}
                                        -- if y is 0
  where tiles = map (addToLoc (0,-1)) $ filter (removeSideFilter (-1,0)) (m'tiles m)
        locFixedTilesMap = M.fromList $ tozip t'loc id tiles

removeSouthSide :: Model -> Model
removeSouthSide m@Model{m'outDims=(w,h)} = m{m'board=locFixedTilesMap,
                m'outDims=(w,h-1)}
                                        -- if y is max
  where tiles = filter (removeSideFilter (-1,h-1)) (m'tiles m)
        locFixedTilesMap = M.fromList $ tozip t'loc id tiles

removeWestSide :: Model -> Model
removeWestSide m@Model{m'outDims=(w,h)} = m{m'board=locFixedTilesMap,
                m'outDims=(w-1,h)}
                      -- if x is max
  where tiles = map (addToLoc (-1,0)) $ filter (removeSideFilter (0,-1)) (m'tiles m)
        locFixedTilesMap = M.fromList $ tozip t'loc id tiles

removeEastSide :: Model -> Model
removeEastSide m@Model{m'outDims=(w,h)} =m{m'board=locFixedTilesMap,
                m'outDims=(w-1,h)}
                        -- if x is 0
  where tiles = filter (removeSideFilter (w-1,-1)) (m'tiles m)
        locFixedTilesMap = M.fromList $ tozip t'loc id tiles
-- TODO: remove N,W,S,E sides

-- boardFilter m' = fst ds * snd ds > 4
--   where ds = M.outDims m'

-- shrinkf :: (Model -> Model) -> Model -> [Model]
-- shrinkf sf m = concatMap shrinkf subterms ++ subterms
--   where subterms = filter boardFilter $ sf m

shrinkm :: Model -> [Model]
shrinkm m = recsubterms ++ subterms
  where (w,h) = m'outDims m
        ns = if h > 0 
                then [removeSouthSide m, removeNorthSide m]
                else []
        ew = if w > 0
                then [removeEastSide m, removeWestSide m]
                else []
        subterms = ns ++ ew
        recsubterms = concatMap shrinkm subterms

instance Arbitrary Model where
  arbitrary = do
    wh <- genWH :: Gen Point
    rands <- infiniteList :: Gen [Int]
    let ps = getSimplePatterns
    return (setupModel rands wh ps 4)

  shrink m = shrinkm m

genSteppedModel :: Gen Model
genSteppedModel = do
  m <- arbitrary :: Gen Model
  n <- chooseInt (0, uncurry (*) (m'outDims m)) :: Gen Int
  return (stepModelNtimes n m)

unzip1 = uncurry

stepModelNtimes :: Int -> Model -> Model
stepModelNtimes n m = until ((== n) . m'step) step m
