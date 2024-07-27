module PatternsSpec where

import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M
import qualified Data.Set as S
import Patterns
import Data.List (group,sort,mapAccumL)
import qualified Graphics.Gloss as GL

import TestUtils
import Test.QuickCheck.Property ((===), (==>), forAll)
import Data.Tuple (swap)
import Utils (tozip, mapTuple)
import Codec.Picture.Types (Image(..))
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Gen (chooseInt)

n = 3
img = genFakeImage n
ps = initializePatternsFromImage img n
probps = addProbabilitiesToPatterns ps
(idps,idVec) = addIdsAndDefaultAdjMapToPatterns probps

spec :: Spec
spec = describe "Patterns" $ do
    prop "should not return nothing" $ do
        forAll genImage $ \img ->
          forAll (chooseInt (1,uncurry min $ dims img)) $ \n -> do
              not $ null (initializePatternsFromImage img n)

    describe "initializePatternsFromImage" $ do
      it "should have p'vec" $ do
        let vps = map (not . VS.null . p'vec) ps :: [Bool]
        foldTrue vps `shouldBe` True
      it "pvec should be length n * n * 4" $ do
        let len = n * n * 4
        let vps = map ((len==) . VS.length . p'vec) ps
        foldTrue vps `shouldBe` True
      it "should have p'hash" $ do
        let hps = map ((/=0) . p'hash) ps
        foldTrue hps `shouldBe` True
      it "should have loc" $ do
        let lps = map ((/=(-1,-1)) . p'loc) ps
        foldTrue lps `shouldBe` True
      it "no hashes should be zero" $ do
        let hshs = filter (/=0) (map p'hash ps)
        length hshs `shouldBe` length ps
      it "# of unique hashes should not be zero" $ do
        let hshset = S.fromList $ map p'hash ps
        S.size hshset `shouldNotBe` 0

    describe "addProbabilitiesToPatterns" $ do
      it "no probabilities should be zero" $ do
        let pps = map ((/=0.0) . p'prob) probps
        foldTrue pps `shouldBe` True

    describe "addIdsAndDefaultAdjMapToPatterns" $ do
      it "all ids should be unique to a hash" $ do
        let idhshs = zip (map p'hash idps) (repeat 0)
        let idhshMp = M.fromListWith (const $ const 0) idhshs
        let lenHshsNoZeros = length $ filter (==0) (M.elems idhshMp)
        lenHshsNoZeros `shouldBe` M.size idhshMp
      let ids = map p'id idps
      it "all ids should be >= 0" $ do
        let lt0Ids = filter (<0) ids
        length lt0Ids `shouldBe` 0
      it "number of unique ids should not be 0" $ do
        let numUniqIds = S.size $ S.fromList ids
        numUniqIds `shouldSatisfy` (> 1)
      it "ids should go 0-num ids" $ do
        let uniqIds = S.toAscList $ S.fromList ids
        let numUniqIds = length uniqIds
        uniqIds `shouldBe` [0..numUniqIds-1]
      it "max id should not be 0" $ do
        let maxId = maximum ids
        maxId `shouldNotBe` 0
      it "number of ids == number of unique patterns (by hash)" $ do
        let numUniqIds = S.size $ S.fromList ids
        let numUniqHashes = S.size $ S.fromList (map p'hash idps)
        numUniqIds `shouldBe` numUniqHashes
      let defadjMap = map p'adjacents idps
      it "default adj map should be 4 long" $ do
        let lenAdjMap = map M.size defadjMap
        lenAdjMap `shouldBe` replicate (length lenAdjMap) 4
      let dirAdjs = concatMap M.elems defadjMap :: [V.Vector Double]
      it "all adj dir vecs should be the same length" $ do
        let lenAdjs = map V.length dirAdjs :: [Int]
        let lenFst = head lenAdjs :: Int
        lenAdjs `shouldBe` replicate (length dirAdjs) lenFst
      it "all adj dir vecs should not have len 0" $ do
        let lenAdjs = map V.length dirAdjs :: [Int]
        sum lenAdjs `shouldNotBe` 0
      it "adj in all directions should be zero" $ do
        let foldEqZer fs = foldr (+) 0.0 fs :: Double
        let zerDirAdjs = map foldEqZer dirAdjs :: [Double]
        zerDirAdjs `shouldBe` replicate (length dirAdjs) 0.0

    let nebps = addNeighborsToPatterns (idps,idVec)
    describe "addNeighborsToPatterns" $ do
      let pidMap = mapPattern idps p'loc p'id
      it "# of unique ids in pidMap shouldNotBe 0" $ do
        let idset = S.fromList $ M.elems pidMap
        S.size idset `shouldNotBe` 0
      let dirAdjs = concatMap (M.elems . p'adjacents) nebps :: [V.Vector Double]
      -- it "length pidMap elems should equal max id" $ do
      --   let ids = M.elems pidMap
      --   -- let max = foldr (\a b -> if a > b then a else b) 0 ids
      --   let maxid = foldr max 0 ids
      --   length ids `shouldBe` maxid
      it "all adj dir vecs should be same length as returned idVec" $ do
        let lenAdjs = map V.length dirAdjs :: [Int]
        lenAdjs `shouldBe` replicate (length dirAdjs) (length idVec)
      it "every pattern should have some neighbors" $ do
        let foldAdd fs = foldr (+) 0.0 fs :: Double
        let zerDirAdjs = map foldAdd dirAdjs :: [Double]
        let nEqZero = filter (0.0 /=) zerDirAdjs
        length nEqZero `shouldNotBe` 0

    let nodupps = mergeDuplicatePatterns nebps
    describe "mergeDuplicatePatterns" $ do
      it "no patterns should have same id" $ do
        let ids = S.fromList $ map p'id nodupps
        length nodupps `shouldBe` S.size ids
      it "no patterns should have same hash" $ do
        let hashs = S.fromList $ map p'hash nodupps
        length nodupps `shouldBe` S.size hashs
    let picps = addPicturesToPatterns nodupps
    describe "addPicturesToPatterns" $ do
      it "no picture should be blank" $ do
        let blanks = filter (==GL.Blank) $ map p'pic picps
        length blanks `shouldBe` 0


flipMap mp = M.fromList $ map swap $ M.assocs mp

tonXn wh n = mapTuple submodn wh
  where submodn a = a - (mod a n)

allHashesUniqueToIds ps =
  let idhshmp = M.fromList $ tozip p'id p'hash ps
      hshidmp = M.fromList $ tozip p'hash p'id ps
      flpidhshmp = flipMap idhshmp
  in
      flpidhshmp === hshidmp
