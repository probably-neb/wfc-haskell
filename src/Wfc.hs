{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Wfc where

import GHC.Stack
import Debug.Trace
import Graphics.Gloss hiding (Vector,Point)
import System.Random
import Control.Monad.Trans.State

import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (catMaybes,mapMaybe)
import Data.List (sort, unfoldr,uncons, intersperse)
import qualified Data.Bifunctor as Bi (second, first)

import Patterns
import Point
import Utils
import Entropy
import qualified Stack as S (empty)
import Stack
import Control.Monad ((<=<))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import qualified Data.Foldable as F

type Dir = Point

collapsed_val :: Double
collapsed_val = 0.0

data Model = Model {
  m'outDims :: WdHt,
-- how we know which tile is where
  m'board :: Map Point Tile,
  m'patterns :: Vector Pattern, -- each pattern is at the index of their id
  m'probVec :: IdVec,
  m'adjVec :: Vector (Vector Double),
-- stores the entropies for all points for easier retrieval of the min (non zero)
-- would be usefull if values could be updated
  -- m'entropyHeap :: Heap (Entry Double Point),
  m'n :: Int,
  m'len :: Int, -- the len of the various pattern info vectors
  m'done :: Bool,
  m'step :: Int,
  m'stack :: Stack Point,
  m'rands :: [Int],
  m'prev :: Model -- previous model (where tile was collapsed)
}

data Tile = Tile {
  -- Map of hashed patterns too frequencies.
  t'domain :: IdVec, -- only 1s and 0s
  -- Map of each cardinal dir [(0,1),(0,-1)...] to the allowed neighbors
  -- The point of the tile
  t'loc :: Point,
  t'pic :: Picture,
  t'collapsed :: Bool, -- could check domain or pic for what will change for them but this is nicer on the brain
  -- the picture for this tile initialized as default wireframe square, then set to correct image once collapsed
  t'entropy :: Double,
  t'rand :: Int -- Used to create s
}

randomPatternHeuristic :: Int -> IdVec -> Int
randomPatternHeuristic randKey dom = case validIndeces V.!? index of
                               Just i -> i
                               Nothing -> error "not valid index in randomPatternHeuristic"
  where
    validIndeces = V.elemIndices 1.0 dom :: Vector Int
    index = fst $ if V.length validIndeces == 0
                    then error "min entropy tile has no entropy"
                    else let range = (0,V.length validIndeces-1)
                             gen = mkStdGen randKey
                         in
                             randomR range gen


mostProbablePatternHeuristic :: IdVec -> IdVec -> Int
mostProbablePatternHeuristic probs dom = V.maxIndex (andIdVecs dom probs)

------------------------------------------------------------------------------------------

entropyOfDomain :: IdVec -> IdVec -> Double
entropyOfDomain dom probs = Entropy.entropy probVec
  where probVec = andIdVecs dom probs

minEntropy' :: [(Double,Tile)] -> Maybe Tile
minEntropy' [] = Nothing
minEntropy' ts = Just . snd $ minimum ts


findMinEntropy :: [Tile] -> Maybe Tile
findMinEntropy ts = minEntropy'
                    $ filterNE collapsed_val fst
                    $ filter (not . t'collapsed . snd)
                    $ tozip t'entropy id ts

updateTile :: Tile -> State Model ()
updateTile ts = do m@Model {m'board=board} <- get
                   put m{m'board=insertTile board ts}

updateTiles :: Foldable f => f Tile -> State Model ()
updateTiles ts = do m@Model {m'board=board} <- get
                    put m{m'board=foldl insertTile board ts}

-- additionally updates tile pics
updatePropogatedTiles :: [Tile] -> State Model ()
updatePropogatedTiles ts =
  do pats <- m'patterns <$> get
     n <- m'n <$> get
     let updPicTs = map (\ t@Tile{t'domain=tdom}
                            -> t{t'pic=getTilePic pats n tdom}) ts
     updateTiles updPicTs

domainInDirection :: IdVec -> Int -> Vector IdVec -> Dir -> IdVec
domainInDirection tdom vlen adjVec dir =
  V.foldr orIdVecs (falseIdVec vlen)
  $ V.map (\idx -> adjVec V.! indexAdjVec vlen dir idx)
  $ V.findIndices ivB tdom

directionalDomains :: IdVec -> Int -> Vector IdVec -> [(Dir,IdVec)]
directionalDomains tdom vlen adjVec =
  tozip id (domainInDirection tdom vlen adjVec) cardinalDirs

-- domains of adjacent tiles
surroundingDomains :: Point -> Map Point Tile -> [(Dir,Maybe Tile)]
surroundingDomains tloc board =
  map (Bi.second (board M.!?))
  $ tozip id (add tloc) cardinalDirs

combineSurAndDirDomains :: IdVec -> [(Dir,Maybe Tile)] -> [(Dir,IdVec)] -> [Tile]
combineSurAndDirDomains probs surdoms dirdoms =
  catMaybes
  $ zipWith (\mybadj dirdom ->
    case mybadj of
      Just adj@Tile{t'domain=adjdom}
        -> let newdom = andIdVecs adjdom dirdom in
             if newdom /= adjdom && not (t'collapsed adj)
               then
                 Just adj {
                        t'domain=newdom,
                        t'entropy=entropyOfDomain newdom probs
                      }
               else
                 Nothing
      Nothing
        -> Nothing)
  (map snd $ sort surdoms) (map snd $ sort dirdoms)

stackPush :: (a -> Stack Point -> Stack Point) -> a -> State Model a
stackPush pushf ps = do m@Model{m'stack=stack} <- get
                        put m{m'stack=pushf ps stack}
                        return ps

stackPop :: State Model Point
stackPop = do m@Model{m'stack=stack} <- get
              let (p,remstack) = pop stack
              put m{m'stack=remstack}
              return p

unintentionallyCollapsed ts = foldl (\bl tl -> (t'entropy tl == 0.0) || bl) False ts

-- pattern match the maybe away because rands is infinite
assUncons :: [Int] -> Tile -> ([Int],Tile)
assUncons rs t = let (Just (hed,tal)) = uncons rs in
                     (tal,t{t'rand=hed})

updateRands :: Map Point Tile -> [Int] -> ([Int],Map Point Tile)
updateRands board rands = M.mapAccum assUncons rands board

restoreModel :: [Int] -> Model -> Model
restoreModel newrands m@Model{m'rands=rnds,m'prev=prev@Model{m'board=board}} =
  let (newrnds, newboard) = updateRands board newrands in
    prev{m'board=newboard,m'rands=newrnds}

restoreM :: State Model ()
restoreM = do
  oldrands <- ((drop 10) . m'rands) <$> get
  let newrands = (drop 10 oldrands) :: [Int]
  modify $ restoreModel newrands
  return ()

propogate :: State Model (Stack Point)
propogate = do m@Model{m'board=board,m'probVec=probs} <- get
               tloc <- stackPop
               let t@Tile{t'domain=tdom} = board M.! tloc
                   updatedAdjs =
                     combineSurAndDirDomains probs
                       (surroundingDomains tloc board)
                       (directionalDomains tdom (m'len m) (m'adjVec m)) ::[Tile]
                   updatedAdjLocs = map t'loc updatedAdjs
               if unintentionallyCollapsed updatedAdjs
                  then do
                    -- restore
                    -- return Stack.empty -- will be ignored
                    -- stackPush pushl updatedAdjLocs
                    -- updatePropogatedTiles updatedAdjs
                    -- m'stack <$> get -- return stack
                    markDone
                    return Stack.empty -- will be ignored
                  else do
                    stackPush pushl updatedAdjLocs
                    updatePropogatedTiles updatedAdjs
                    m'stack <$> get -- return stack

recursivePropogate :: State Model Bool
recursivePropogate = do stack <- m'stack <$> get
                        if Stack.null stack
                           then
                             return True
                           else
                           do propogate
                              recursivePropogate

collapseTileM :: Tile -> Vector Pattern -> (IdVec -> Int) -> Tile
collapseTileM t@(Tile{t'domain=dom}) patterns heuristic =
              let index = heuristic dom in
                t {
                    t'domain = V.map (const 0.0) dom V.// [(index,1.0)],
                    t'collapsed = True,
                    t'pic = p'pic (patterns V.! index),
                    t'entropy = collapsed_val
                  }

markDone :: State Model Bool
markDone = do modify (\m -> m{m'done=True})
              return True

collapseMinEntropyTile :: State Model (Maybe Point)
collapseMinEntropyTile =
  do m@Model{m'patterns=patterns} <- get
     case findMinEntropy (m'tiles m) of
            Just minEntTile ->
              do patterns <- m'patterns <$> get
                 let oldDom = t'domain minEntTile
                 let index = heuristic minEntTile
                 let t = minEntTile {
                        t'domain = V.map (const 0.0) oldDom V.// [(index,1.0)],
                        t'collapsed = True,
                        t'pic = p'pic (patterns V.! index),
                        t'entropy = collapsed_val
                      }
                 updateTile t
                 let tloc = t'loc t
                 stackPush push (tloc)
                 modify (\mdl->mdl{m'prev=m})
                 return (Just tloc)
            Nothing -> do markDone
                          return Nothing
    where
      --heuristic = (mostProbablePatternHeuristic)
      heuristic = \t -> randomPatternHeuristic (t'rand t) (t'domain t)

incStepCounter :: State Model Int
incStepCounter = do m@Model{m'step=prev} <- get
                    let cur = prev + 1
                    put m{m'step=cur}
                    return cur

observeNextM :: State Model Bool
observeNextM = do stack <- m'stack <$> get
                  done <- m'done <$> get
                  if Stack.null stack && not done
                     then do
                       maybtloc <- collapseMinEntropyTile
                       case maybtloc of
                         Just tloc -> do incStepCounter
                                         return False

                         Nothing -> do return True
                     else do
                       propogate
                       m'done <$> get

observeUntilCollapse :: State Model Bool
observeUntilCollapse = do observeNextM
                          stack <- m'stack <$> get
                          if not (Stack.null stack)
                             then do
                               observeUntilCollapse -- recursive call
                               m'done <$> get
                             else do
                               m'done <$> get -- return done

-- takes two ignored inputs from Gloss (viewport and time since start)
stepM :: a -> b -> Model -> Model
stepM _ _ m = execState observeNextM m

step :: Model -> Model
step = stepM 0 0

stepMaybe :: Model -> Maybe Model
stepMaybe mi | m'done mf = Nothing
             | otherwise = Just mf
  where mf = step mi

stepUntilDone :: Model -> [Model]
stepUntilDone = unfoldr (fmap dup . stepMaybe)
-----------------------------------------------------------------------------

data TextColor = Green | Yellow | Magenta

fmt :: (Show a1, Show a2) => a1 -> a2 -> [Char]
fmt n a = "\ESC[" ++ show n ++ "m" ++ show a ++ "\ESC[0m"

fg :: Show a => TextColor -> a -> String
fg Green a = fmt 32 a
fg Yellow a = fmt 33 a
fg Magenta a = fmt 35 a

-- instance Show Tile where
--   show t = concat [if t'collapsed t then "\ESC[92m" ++ tstr else if fromIntegral (round (t'entropy t)) /= t'entropy t || V.sum (t'domain t) /= fromIntegral (V.length (t'domain t)) then "\ESC[93m" ++ tstr else "",", H(",show (t'entropy t),")",show (allowedInDomain(t'domain t)), "\ESC[0m"]
--     where tstr = concat ["Tile: ",  show $ t'loc t]

instance Show Tile where
  show t = unwords [ fg Magenta "{|",
      show (t'loc t),
      "H:"++show (t'entropy t),
      if t'collapsed t
        then fg Green "COLLAPSED"
        else "",
      "D:" ++ show (allowedInDomain (t'domain t)),
      fg Magenta "|}"]

instance Show Model where
  show m = concat ["Model{Board: ", show (m'tiles m),"Step:",show (m'step m),"}"] --," patterns: ",show (m'patterns m)

instance Eq Model where
  m1 == m2 = m'board m1 == m'board m2
             && M.isSubmapOfBy tEq (m'board m1) (m'board m2)
             && m'adjVec m1 == m'adjVec m2
             && m'patterns m1 == m'patterns m2
             && m'probVec m1 == m'probVec m2
             && m'done m1 == m'done m2

instance Eq Tile where
  -- so tiles are updated when inserted in board
  t1 == t2 = t'loc t1 == t'loc t2

-- for hard equality of tiles
tEq :: Tile -> Tile -> Bool
tEq t1 t2 = t'domain t1 == t'domain t2
            && t'collapsed t1 == t'collapsed t2
            && t'loc t1 == t'loc t2
            && t'entropy t1 == t'entropy t2
            -- && t'pic t1 == t'pic t2

instance Ord Tile where
  compare t1 t2 = compare (t'loc t1) (t'loc t2)

m'tiles :: Model -> [Tile]
m'tiles = M.elems . m'board

allowedInDomain :: IdVec -> [Int]
allowedInDomain d = V.toList $ V.elemIndices 1.0 d

setDefaultBoard :: [Int] -> Model -> Model
setDefaultBoard rands m =
  m {m'board = M.fromList
  [(pnt, Tile {t'loc=pnt,
               t'domain=defaultDomainVec,
               t'entropy=defaultEntropy,
               t'collapsed=False,
               t'pic = defaultPic,
               t'rand = rand
              })
  | i <- [0..numTiles-1],
    let pnt = coords !! i,
    let rand = rands !! i]}
  where
    defaultDomainVec = V.force $ V.replicate (V.length (m'probVec m)) 1.0
    defaultEntropy = entropyOfDomain defaultDomainVec (m'probVec m)
    n = m'n m
    -- defaultPic = getTilePic (m'patterns m) n defaultDomainVec
    defaultPic = defaultWirePic (fromIntegral n)
    dims' = m'outDims m
    coords = getCoords dims'
    numTiles = uncurry (*) dims' -- w * h

getPatternIdMap :: [Pattern] -> IntMap Pattern
getPatternIdMap ps = intMapPattern ps p'id id

getPatterns :: [Pattern] -> Vector Pattern
getPatterns ps = V.fromList (I.elems (getPatternIdMap ps))

insertTile :: Map Point Tile -> Tile -> Map Point Tile
insertTile mp t = M.insert (t'loc t) t mp

insertMultiple :: Foldable f => Map Point Tile -> f Tile -> Map Point Tile
insertMultiple = foldl insertTile

indexAdjVec :: Int -> Point -> Int -> Int
indexAdjVec len dir pid = (len * didx) + pid
  where didx = dirToIdx dir

getAdjVec :: Vector Pattern -> Vector IdVec
getAdjVec pv = adjVec
  where len = V.length pv
        baseAdjVec = V.replicate (4*len) V.empty
        idAdjs =     [(p'id p, p'adjacents p) | p <- V.toList pv]
        idAdjElems = [(id',M.assocs aMap) | (id',aMap) <- idAdjs]
        idDirAdjs  = concatMap (\(id',elms) -> map (\e -> (id',e)) elms) idAdjElems
        idxAdjs = [(indexAdjVec len dir pid, idv) | (pid,(dir,idv)) <- idDirAdjs]

        adjVec = baseAdjVec V.// idxAdjs

setupModel :: [Int] -> WdHt -> [Pattern] -> Int -> Model -- patterns are assumed to be unique
setupModel rands wdHt ps n =
  setDefaultBoard rands -- relies on initialied values
  $ Model {m'board=M.empty,
           m'patterns=patterns,
           m'outDims=wdHt,
           m'n = n,
           m'probVec=probVec,
           m'adjVec=adjVec,
           m'len = len,
           m'done=False,
           m'step=0,
           m'stack=Stack.empty,
           m'prev=undefined,
           m'rands=rands
          }
  where patterns = getPatterns ps
        probVec = V.map p'prob patterns
        len = V.length patterns
        adjVec = V.force $ getAdjVec patterns :: Vector (Vector Double)

initializeStack :: Point -> State Model ()
initializeStack p = do m <- get
                       put m{m'stack=Stack.singleton p}

adjMapToVector :: Map Point IdVec -> Vector (Vector Double)
adjMapToVector adjMp = dirVec V.// dirElems
  where
        elems = M.assocs adjMp
        dirElems = map (Bi.first dirToIdx) elems
        dirVec = V.replicate 4 V.empty

-- tileSetPic :: Picture -> Tile -> Tile
-- tileSetPic p t = t {t'pic = p}

avg :: (Foldable t, Integral i) => t i -> Maybe i
avg as | Prelude.null as = Nothing
       | otherwise = Just (div (sum as) (fromIntegral $ length as))

mergeImageVecs :: V.Vector (VS.Vector Word8) -> VS.Vector Word8
mergeImageVecs vs | V.length vs == 1 = V.head vs
mergeImageVecs vs = VG.unfoldr (\svs -> do
    hdtls <- VG.mapM VG.uncons svs
    (hds,tls) <- Just $ bigzip VG.cons VG.cons (VG.empty, VG.empty) hdtls
    avgt <- avg hds
    return (avgt,tls)
  ) vs

getTilePic :: V.Vector Pattern -> Int -> IdVec -> Picture
getTilePic pv n tdom =
  vectorToPicture
    ( mergeImageVecs
      $ VG.map p'vec
      $ V.catMaybes
      $ andNIdVec tdom pv
      ) (n,n)

defaultWirePic :: Float -> Picture
defaultWirePic n = color black $ rectangleWire n n

defaultPixelPic :: Picture
defaultPixelPic = color black $ rectangleSolid 100.0 100.0
