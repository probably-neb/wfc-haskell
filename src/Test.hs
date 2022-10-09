module Test where

import Prelude
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Word
import Control.Monad.Trans.State
import Data.List (sort,nub)
import qualified Data.Set as S

import Entropy
import Wfc
import Patterns
import Point
import Codec.Picture
import Stack

assert :: Bool -> String -> Bool
assert False msg = error msg
assert x _ = x -- return true value

testModel :: Model
testModel = initModel
  where
    img = genFakeImage 20 genFakePixel2
    plst =  getPatternsFromImage img 3
    initModel = setupModel (67,67) plst 3

printF :: Foldable f => f String -> IO ()
printF arr = putStrLn $ foldr ((++)) "" arr

testInputModel :: IO Model
testInputModel = do img <- loadInput "inputs/celtic.png"
                    let n = 100
                    let ps = getPatternsFromImage img n
                    let m = setupModel (25,25) ps n
                    return m
                    -- return (stepM 1 1 m)
                    --

testPatternMerge :: IO Bool
testPatternMerge = do let img = genFakeImage 100 genFakePixel
                          ips = initializePatternsFromImage img 3
                      -- sum hashes to lazy load them

                          lenips = length ips
                          ass1 = assert (lenips > 2) "Didnt find 2 patterns in checkerboard"
                          idps = addIdsAndDefaultAdjMapToPatterns ips
                      let ids = map p'id idps
                          numUniquePats = S.size $ S.fromList $ ids
                      printF ["#unique patterns: ", show numUniquePats,"\n","#total patterns: ",show lenips]
                      let pps = addNeighborsToPatterns idps
                          ass3 = foldl1 (&&) ( map (\p -> assert (M.size (p'adjacents p) == 4) "adjacents len not 4") pps)
                          mergedPatterns = mergeDuplicatePatterns pps
                      printF["#ids = #patterns: ", show ((S.size (S.fromList $ map p'id idps)) == length mergedPatterns)]

                      let allAdjs = map (M.elems . p'adjacents) mergedPatterns
                      printF ["#of directional domains: ",show allAdjs]
                      let allDomsLen1 = Prelude.null (filter (\p -> Prelude.null (filter (\adj -> possibilities adj == 0) (M.elems (p'adjacents p)))) mergedPatterns)
                      printF ["All doms len 1 : ", show allDomsLen1]
                          -- mergedPatterns = [1,2]
                      let allDomsLen1 = True
                      return (length mergedPatterns == 2)

testPatternGen :: IO ()
testPatternGen = do img <- loadInput "inputs/celtic.png"
                    let n = 15
                    let ps = getPatternsFromImage img n
                    let patternsWithNebs = filter (\p -> (length $ filter (\dom'->possibilities dom' /= 0) (M.elems $ p'adjacents p)) >= 2) ps
                    printF ["Num patterns: ", show $ length ps,"\n","Num Patterns with >= 2 adjacents : ",show $ length patternsWithNebs]


testCollapse :: State Model Point
testCollapse = do m@Model{m'board=board} <- get
                  let tiles = M.elems board
                      different_tiles = filter (V.elem 0.0 . t'domain) tiles
                  return (0,0)

testProp :: Point -> State Model (Stack Point,Tile,[Tile])
testProp p = do m@Model{m'outDims = wdht,m'board=board}<- get
                let stack = push p Stack.empty
                let adjacentPnts = adjacents 1 wdht p
                let adjacents = map (board M.!) adjacentPnts
                let adjacentDoms = map t'domain adjacents
                let (updatedStack,updatedModel) = runState (propogate stack) m
                let t = board M.! p
                put updatedModel
                return (updatedStack,t,adjacents)

hl :: IO ()
hl = putStrLn "----------------------------------------------------"

adjInDir :: Dir -> Point -> M.Map Point Tile -> Tile
adjInDir dir tloc board = case board M.!? adjPnt of
                            Just t ->  t
                            Nothing -> error "No tile in said direction"
                        where adjPnt = add dir tloc

possibilities :: IdVec -> Int
possibilities dom = V.length $ V.elemIndices 1.0 dom

testStep :: IO ()
testStep = do m <- testInputModel
              -- test collapsing a tile
              let (collapsedTileLoc, collapsedModel@Model{m'board=collapsedBoard, m'outDims=wdht}) = runState collapseMinEntropyTile m
                  collapsedTile = collapsedBoard M.! collapsedTileLoc
              print $ "Domain of collapsed Tile: " ++ show (V.length $ allowedInDomain $ t'domain collapsedTile)

              hl
              -- test domain in direction 
              let collapsedPatternIndex = case V.elemIndex 1.0 (t'domain collapsedTile) of
                                            Just idx -> idx
                                            Nothing -> error "not pattern index"
              let collapsedPattern = (m'patterns m) V.! collapsedPatternIndex
              let cPadjMap = p'adjacents collapsedPattern
              let dirDoms = [(dir, getDomainInDirection collapsedTile dir collapsedModel) | dir <- (adjacentDirs 1 wdht collapsedTileLoc)]
              let doms = map (\(dir',dom') -> (dir', (cPadjMap M.! dir'))) dirDoms
              putStrLn $ "Collapsed Tile Domains in Directions:" ++ show (length doms)
              mapM_ (\(dir',dom') -> putStrLn $ "Pattern Domain In dir " ++ show dir' ++ " : " ++ show (possibilities dom')) doms
              mapM_ (\(dir',dom') -> putStrLn $ "Tile Domain In dir " ++ show dir' ++ " : " ++ show (possibilities dom')) dirDoms

              -- propogate one step using collapsed tile
              let ((nStack,unchangedTile,prePropped),proppedModel@Model{m'board=proppedModelBoard}) = runState (testProp collapsedTileLoc) collapsedModel

              hl
              putStrLn $ "Tdom unchanged: " ++ show ((t'domain unchangedTile) == (t'domain collapsedTile))

              -- compare stack and adjacents to make sure they got added
              let (sortedStack,sortedPrePropped) = (sort (Stack.toList nStack), sort (map t'loc prePropped))
              let preProppedEqStack = sortedStack == sortedPrePropped
              putStr $ "Propped Stack == prePropped adjacents: " ++ show preProppedEqStack
              if preProppedEqStack
                 then putStrLn "" -- just make new line
                 else putStrLn $ "FALSE ==> (Stack,prePropped) = " ++ show (sortedStack,sortedPrePropped)

              hl
              -- see if domains where updated correctly
              let collapsedNebDoms = [(dir, t'domain (adjInDir dir collapsedTileLoc collapsedBoard)) | dir <- (adjacentDirs 1 wdht collapsedTileLoc)]
              let proppedNebDoms = map (\(dir',_) -> (dir', t'domain (adjInDir dir' collapsedTileLoc proppedModelBoard))) collapsedNebDoms

                    -- assume default is set for neighbors
              let defaultDom = V.map (const 1.0) (m'patterns proppedModel)
                  adjChangedDoms = filter (\(dir',dom') -> dom' /= defaultDom) proppedNebDoms
              printF ["Num adjacents: ",show $ length collapsedNebDoms,"\n","Num changed domains: ",show $length adjChangedDoms]

              let changeFromCollapsedToPropped = map (\(dir',dom') -> (dir', possibilities dom' - possibilities (t'domain (adjInDir dir' collapsedTileLoc collapsedBoard)))) adjChangedDoms
              printF ["Length of possible patterns = ",show $ possibilities defaultDom]
              mapM_ (\(d',b') -> printF ["Changed Dom in dir: ",show d'," : ",show b']) changeFromCollapsedToPropped

              hl
              -- collapse again
              let entropyNebs = map (\(dir',_) -> (dir',t'entropy (adjInDir dir' collapsedTileLoc proppedModelBoard))) adjChangedDoms
              let dirDomains = [(dir, getDomainInDirection collapsedTile dir proppedModel) | dir <- (adjacentDirs 1 wdht collapsedTileLoc)]
              let possibilitiesNebs = map (\(dir',dom') -> (dir',possibilities dom')) adjChangedDoms
              let possibilitiesInDir = map (\(dir',dom') -> (dir',possibilities dom')) dirDomains
              mapM_ (\(d,ent) -> printF ["New entropy in dir: ",show d," = ",show ent]) entropyNebs
              mapM_ (\(d,pos) -> printF ["Possibilities in dir: ",show d," = ",show pos]) possibilitiesInDir

              print "Done!"


testModelNoNaNs :: IO ()
testModelNoNaNs = do img <- loadInput "inputs/circuit.png"
                     let ps = getPatternsFromImage img 3
                     let m = setupModel (100,100) ps 15
                     let m1 = stepM 1 1 m
                     if testNoNaNs m1
                        then print $ "No NaNs" ++ show m1
                        else print "Not gonna print this"

testNoNaNs :: Model -> Bool
testNoNaNs m = bool
  where ts = M.elems (m'board m)
        bools = (map (\t -> assert (not . isNaN $ t'entropy t) "NaN entropy") ts)
        bool = foldr1 (&&) bools

stringifyPatternAnnoyingly :: Pattern -> String
stringifyPatternAnnoyingly p = (show (p'vec p)) ++ (show (p'loc p)) ++ (show (p'rc p)) ++ (show (p'n p)) ++ (show (p'id p)) ++ (show (p'hash p)) ++ (show (p'adjacents p)) ++ (show (p'prob p))

testWTFWHYMAPNOWORK :: IO ()
testWTFWHYMAPNOWORK = do img <- loadInput "inputs/circuit.png"
                         let ps = getPatternsFromImage img 10
                         let outs = map stringifyPatternAnnoyingly ps
                         print $ length $ map length outs
                         print $ length ps

printIOField :: Show a => IO Model -> (Model -> a) -> IO ()
printIOField mm f = do m <- mm
                       print $ show (f m)

loadCircuit :: IO Model
loadCircuit = do img <- loadInput "inputs/circuit.png"
                 let ps = getPatternsFromImage img 15
                 let m = setupModel (50,50) ps 15
                 return m

genFakePixel :: Int -> Int -> PixelRGBA8
genFakePixel x y = p
  where
    [c1,c2] = [[0,184,0, 1],[184,0,0, 1]]
    fromCoord x' y'
      | even x' /= even y' = c1
      | otherwise = c2
    [r,g,b,a] = map fromIntegral $ fromCoord x y :: [Word8]
    p = PixelRGBA8 r g b a

genFakePixel2 :: Int -> Int -> PixelRGBA8
genFakePixel2 x y = p
  where
    c = fromIntegral $ abs x - y
    p = PixelRGBA8 c c c 1

genFakeImage :: Int -> (Int -> Int -> PixelRGBA8) -> Image PixelRGBA8
genFakeImage dim gen= generateImage gen dim dim

genFakePatterns :: Int -> [Pattern]
genFakePatterns num = getPatternsFromImage img n
  where
    n = 3
    img = genFakeImage (div num n ^ 2) genFakePixel2

