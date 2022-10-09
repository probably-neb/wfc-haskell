module WfcSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Modifiers (NonEmptyList(..),InfiniteList)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M
import qualified Data.Set as S
import Wfc
import Patterns
import Patterns.Internal
import Stack (pop,push,pushl)
import qualified Stack
import Utils
import TestUtils
import Point
import Data.List (group,sort,mapAccumL,unfoldr, nub)

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

xor a b = a /= b

getRands = sample' infiniteList :: IO [[Int]]

inverseDir = mapTuple ((-1) *)

getEitherTile m tloc tdom dir =
  case m'board m M.!? add tloc dir of
   Just t -> domainInDirection (t'domain t) vlen (m'adjVec m) (inverseDir dir)
   Nothing -> tdom
  where vlen = m'len m

tdomIsCompositeOfAdjacents :: Point -> Model -> Bool
tdomIsCompositeOfAdjacents tloc m = tdom == surroundingDomainsComposite
  where tdom = t'domain $ m'board m M.! tloc :: IdVec
        tVec = trueIdVec (m'len m) :: IdVec
        surroundingDomains' = map (getEitherTile m tloc tdom) cardinalDirs
        surroundingDomainsComposite = foldl andIdVecs tVec surroundingDomains'

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

instance Arbitrary Model where
  arbitrary = do
    wh <- genWH :: Gen Point
    rands <- infiniteList :: Gen [Int]
    let ps = getSimplePatterns
    return (setupModel rands wh ps 4)

  shrink m = concatMap shrink subterms ++ subterms
           where rHs = removeHighSide m
                 rLs = removeLowSide m
                 sides = map (m &) [removeEastSide, removeWestSide, removeNorthSide,removeSouthSide]
                 boardFilter m' = M.size (m'board m') >= 4
                 -- subterms = catMaybes [removeEastSide m, rHs,rLs]
                 subterms = filter boardFilter sides

genSteppedModel :: Gen Model
genSteppedModel = do
  m <- arbitrary :: Gen Model
  n <- chooseInt (0, uncurry (*) (m'outDims m)) :: Gen Int
  return (stepModelNtimes n m)

unzip1 = uncurry

stepModelNtimes :: Int -> Model -> Model
stepModelNtimes n m = until ((== n) . m'step) step m

n = 4
dims' = (10,10)
spec :: Spec
spec =  describe "Wfc" $ do
    rands <- runIO getRands
    let m = setupModel (head rands) dims' simplePatterns n
    prop "every tiles domain should be a composite of its surroundings" $
      forAllShrink genSteppedModel shrink $ \mod -> do
            let tileLocs = map t'loc $ m'tiles mod
            let bools =
                  map (`tdomIsCompositeOfAdjacents` mod) tileLocs
            conjoin bools
            -- foldTrue bools `shouldBe` True
            -- mapM (shouldBe True) bools

    -- prop "test tdomIsCompositeOfAdjacents" $ do
    --   \ m ->
    --         let tileLocs = map t'loc $ m'tiles m in
    --         foldTrue (map (`tdomIsCompositeOfAdjacents` m) tileLocs) `shouldBe` True

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

    let m' = step m -- bigger board -- not hand made
    describe "first step" $ do
      it "number of collapsed tiles after first step should be 1" $ do
        let collapsed = filter t'collapsed $ m'tiles m'
        length collapsed `shouldBe` 1
      it "there should be at least 3 different entropies (!0,!default)" $ do
        let grps = nub $ map t'entropy $ m'tiles m'
        length grps `shouldSatisfy` (> 2)
      it "there should be at least 3 different domains (!1,!default)" $ do
        let grps = nub $ map t'domain $ m'tiles m'
        length grps `shouldSatisfy` (> 2)
    let m'' = step m'
    describe "second step" $ do
      let (maybCollapsedLoc,cm'') = runState collapseMinEntropyTile m'
      let collapsedLoc = fromMaybe (-1,-1) maybCollapsedLoc
      let collapsed = m'board cm'' M.! collapsedLoc
      describe "collapsing second tile (not propogating (yet))" $ do
        it "finding min entropy again should do so" $ do
          let minEnt =
                fst $ minimum
                  $ filterNE collapsed_val fst
                    $ tozip t'entropy t'loc (m'tiles m')
          case maybCollapsedLoc of
              Just foundCollapsedLoc ->
                let foundCollapsed = t'entropy (m'board m' M.! foundCollapsedLoc) in
                  foundCollapsed `shouldBe` minEnt
              Nothing -> isJust Nothing `shouldBe` True
        it "second collapsed loc shouldn't equal first" $ do
          let first = t'loc . head . filter t'collapsed $ m'tiles m'
          let second = collapsedLoc
          second `shouldNotBe` first
          second `shouldNotBe` (-1,-1)
        it "number of collapsed tiles after second step should be 2" $ do
          let collapseds = filter t'collapsed $ m'tiles cm''
          length collapseds `shouldBe` 2
        let tileInm' = m'board m' M.! t'loc collapsed
        it "dom of collapsed tile should not have been all in m'" $ do
          length ( V.elemIndices 0.0 $ t'domain tileInm') `shouldNotBe` 0
        it "dom should have changed when collapsed" $ do
          t'domain collapsed `shouldNotBe` t'domain tileInm'
      let pm'' =
            execState recursivePropogate
              cm''{m'stack=Stack.singleton collapsedLoc}
      describe "propogating second step" $ do
        describe "non recursivePropogate" $ do
          let (stack,nrm'') =
                runState recursivePropogate cm''{m'stack=Stack.singleton collapsedLoc}
          it "there should be at least 3 different entropies (!0,!default)" $ do
            let grps = nub $ map t'entropy $ m'tiles nrm''
            length grps `shouldSatisfy` (> 2)
          it "there are be at least 3 different domains (!1,!default)" $ do
            let grps = nub $ map t'domain $ m'tiles nrm''
            length grps `shouldSatisfy` (> 2)
          it "stack shouldn't contain collapsed tiles" $ do
            length (filter t'collapsed $ map (m'board nrm'' M.!) (Stack.asList $ m'stack nrm'')) `shouldBe` 0
          it "no uncollapsed tiles should have no possibilities" $ do
            length (filterE 0.0 t'entropy $ filter (not . t'collapsed) (m'tiles cm'')) `shouldBe` 0

      describe "recursive propogate" $ do
        it "there should be at least 3 different entropies (!0,!default)" $ do
          let grps = nub $ map t'entropy $ m'tiles pm''
          length grps `shouldSatisfy` (> 2)
        it "there are be at least 3 different domains (!1,!default)" $ do
          let grps = nub $ map t'domain $ m'tiles pm''
          length grps `shouldSatisfy` (> 2)
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




      -- let blp = getSimplePattern blank
      --     blid = id' blank
      --     smdl = setupModel (rands !! 3) (2,2) simplePatterns n :: Model
      --     pt = m'board smdl M.! (0,0)
      --     ct = collapseTileM pt (m'patterns smdl) (const blid) -- collapse to blank
      --     mdl = smdl{m'board=M.insert (0,0) ct (m'board smdl)}
      --     stack = Stack.singleton (t'loc ct)
      -- let (stack1,mdl1) = runState propogate mdl{m'stack=stack}
      -- describe "first propogation step" $ do
      --   it "propogate shouldn't change collapsed tile" $ do
      --     m'board mdl1 M.! (0,0) `shouldSatisfy` tEq ct
      --   describe "prop should update cardinal dir neighbors" $ do
      --     let down = m'board mdl1 M.! (0,1)
      --     it "down: domain updated" $ do
      --       t'domain down `shouldBe` allowedNeighbors blank M.! (0,1)
      --     it "down: entropy updated" $ do
      --       t'entropy down `shouldNotBe` t'entropy (m'board mdl M.! (0,1))
      --     let right = m'board mdl1 M.! (1,0)
      --     it "right: domain updated" $ do
      --       t'domain right `shouldBe` allowedNeighbors blank M.! (1,0)
      --     it "right: entropy updated" $ do
      --       t'entropy right `shouldNotBe` t'entropy (m'board mdl M.! (1,0))
      --   describe "stack should contain modified cardinal neighbors" $ do
      --     let stitems = sort $ Stack.asList stack1
      --     it "stack has length 2" $ do
      --       length stitems `shouldBe` 2
      --     it "down" $ do
      --       (0,1) `elem` stitems `shouldBe` True
      --     it "right" $ do
      --       (1,0) `elem` stitems `shouldBe` True
      --     it "not non cardinal neighbor" $ do
      --       (1,1) `elem` stitems `shouldBe` False

      --   it "!rec prop shouldn't update non cardinal neighbors" $ do
      --     let (t,t1) = mapTuple (\m' ->  m'board m' M.! (1,1)) (mdl,mdl1)
      --     tEq t t1 `shouldBe` True
      --   it "board length shouldn't have changed" $ do
      --     M.size (m'board mdl1) `shouldBe` M.size (m'board mdl)
      -- let (nextProp,_) = pop stack1
      -- let (stack2,mdl2) = runState propogate mdl1
      -- describe "second propogation step" $ do
      --   it "shouldn't change collapsed (previously propogated) tile" $ do
      --     m'board mdl2 M.! (0,0) `shouldSatisfy` tEq ct
      --   describe "shouldn't have changed previous stack items" $ do
      --     it "(1,0)" $ do
      --       let (t1,t2) = mapTuple (\m' -> m'board m' M.! (1,0)) (mdl1,mdl2)
      --       tEq t1 t2 `shouldBe` True
      --     it "(0,1)" $ do
      --       let (t1,t2) = mapTuple (\m' -> m'board m' M.! (0,1)) (mdl1,mdl2)
      --       tEq t1 t2 `shouldBe` True
      --   describe "stack" $ do
      --     let stitems2 = Stack.asList stack2
      --     it "stack shouldn't contain collapsed tile" $ do
      --       (0,0) `elem` stitems2 `shouldBe` False
      --     it "stack should only contain one of previous stacks elems" $ do
      --       let hasa = (1,0) `elem` stitems2
      --       let hasb = (0,1) `elem` stitems2
      --       xor hasa hasb `shouldBe` True
      --       hasa `shouldNotBe` hasb
      --     it "stack should be len 1" $ do
      --       length stitems2 `shouldBe` 1
      --     it "stack should not contain corner unchanged in first prop" $ do
      --       (1,1) `elem` stitems2 `shouldBe` False
      --   -- it "dom of corner should have been updated" $ do
      --   --   let dir = (snd nextProp, fst nextProp)
      -- let (finProp,_) = pop stack1
      -- let (stack3,mdl3) = runState propogate mdl2
      -- describe "third propogation step" $ do
      --   it "no tiles should be changed" $ do
      --     mdl3 `shouldBe` mdl2
      --   it "stack should be empty" $ do
      --     stack3 `shouldSatisfy` Stack.null
      -- let (_,rmdl3) = runState recursivePropogate mdl{m'stack=stack}
      -- it "value of recursivePropogate should eq hand propogated" $ do
      --   rmdl3 `shouldBe` mdl3
      -- -- let (_,onmdl3) = runState collapseMinEntropyTile smdl
      -- -- it "value of observeNextM should eq hand propogated" $ do
      -- --   onmdl3 `shouldBe` mdl3
      -- -- it "(0,0) tile last check" $ do
      -- --   let t = m'board mdl3 M.! (0,0)
      -- --   t'domain t `shouldBe` bi
      -- -- it "(1,0) tile last check" $ do
      -- --   let t = m'board mdl3 M.! (1,0)
      -- --   t'domain t `shouldBe` ri -- |! bi
      -- -- it "(0,1) tile last check" $ do
      -- --   let t = m'board mdl3 M.! (0,1)
      -- --   t'domain t `shouldBe` di -- |! bi
      -- -- it "(1,1) tile last check" $ do
      -- --   let t = m'board mdl3 M.! (1,1)
      -- --   t'domain t `shouldBe` alli
