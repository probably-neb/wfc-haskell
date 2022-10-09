module Patterns.Internal where
import Patterns
import qualified Data.Vector as V hiding (map, (++), mapM_)
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M
import Data.Word (Word8)
import Data.Map (Map)
import Point (Point)
import Data.Vector hiding (map, (++), mapM_)
import Codec.Picture (imageData)

getSimplePattern :: SimplePattern -> Pattern
getSimplePattern sp@SimplePattern{name=n, 
                                  allowedNeighbors=an,
                                  id'=i,
                                  imgvec=iv,
                                  prob=prob} = pattern
    where nameToPath nm = "inputs/simple_" ++ nm ++ ".png"
          img = loadInput (nameToPath n)
          pattern = Pattern {
            p'pic = vectorToPicture iv (4,4),
            p'vec = iv,
            p'id = i,
            p'n = 4,
            p'hash = hashPatternVec iv,
            p'adjacents = an,
            p'prob = prob,
            p'rc = (100,100),
            p'loc = (i,0)
                            }

-- data Pattern = Pattern {
--   p'pic :: Picture,
--   p'vec :: Vector Word8,
--   p'loc :: Point,
--   p'rc :: RwCl,
--   p'n :: Int,
--   p'id :: Int,
--   p'hash :: Hash,
--   p'adjacents :: AdjacentsMap,
--   p'prob :: Double 
-- }
getSimplePatterns :: [Pattern]
getSimplePatterns = patterns
  where patterns = map getSimplePattern [blank{prob=nbl/tot},dl{prob=ndl/tot}
                                        ,lu{prob=nlu/tot},ur{prob=nur/tot}
                                        ,rd{prob=nrd/tot}]
        tot = 15
        nbl = 4 -- most probable
        ndl = 3
        nlu = 3
        nur = 2
        nrd = 3


getSpImg sp = ionameVecStr
  where nameToPath nm = "inputs/simple_" ++ nm ++ ".png"
        name' = name sp
        path = nameToPath name'
        ioimg = loadInput path
        ioimgdata = fmap imageData ioimg
        ioStrImgData = fmap show ioimgdata
        ionameVecStr = fmap (name' ++) ioStrImgData


printSimplePatternData :: IO ()
printSimplePatternData = do 
                            bimg <- getSpImg blank
                            dlimg <- getSpImg dl
                            luimg <- getSpImg lu
                            urimg <- getSpImg ur
                            rdimg <- getSpImg rd
                            let iopatterns = [bimg,dlimg,luimg,urimg,rdimg]
                            mapM_ (print) iopatterns

data SimplePattern = SimplePattern {
  id' :: Int,
  name :: String,
  allowedNeighbors :: Map Point (Vector Double),
  imgvec :: VS.Vector Word8,
  prob :: Double
}


blank = SimplePattern {
prob = 0.0,
id' = 0,
name = "blank",
allowedNeighbors = M.fromList [((-1,0),li), --left
                               ((0,1),ui), --up 
                               ((1,0),ri), --right
                               ((0,-1),di) -- down
                              ]
, imgvec = VS.fromList $ map fromIntegral [255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,254,254,255,255,254,255,255,255,254,254,255,254,255,254,255,255,255,255,255,255,255,254,255,255,254,255,255,254,254,255,255,255,255,255,255,255,255,254,255,255,254,255,255] :: VS.Vector Word8
  }

dl = SimplePattern {
prob = 0.0,
id' = 1,
name = "dl",
allowedNeighbors = M.fromList [((-1,0),ri), --left
                               ((0,1),ui), --up 
                               ((1,0),ri |! bi), --right
                               ((0,-1),ui |! bi) -- down
                              ]
, imgvec = VS.fromList $ map fromIntegral [254,254,254,255,254,254,254,255,254,255,255,255,254,255,254,255,1,1,0,255,1,1,1,255,1,0,0,255,255,255,254,255,1,1,0,255,1,0,1,255,1,0,0,255,255,254,255,255,254,255,255,255,0,0,0,255,1,0,0,255,255,255,255,255]
  }

lu = SimplePattern {
prob = 0.0,
id' = 2,
name = "lu",
allowedNeighbors = M.fromList [((-1,0),ri), --left
                               ((0,1),di), --up 
                               ((1,0),ri |! bi), --right
                               ((0,-1),di |! bi) -- down
                              ]
, imgvec = VS.fromList $ map fromIntegral [255,255,254,255,0,0,0,255,1,1,1,255,255,254,255,255,0,0,1,255,1,1,1,255,0,0,1,255,254,254,254,255,0,1,0,255,1,1,0,255,0,0,0,255,254,254,255,255,254,255,255,255,255,255,254,255,254,255,254,255,255,255,255,255]
  }

rd = SimplePattern {
prob = 0.0,
id' = 3,
name = "rd",
allowedNeighbors = M.fromList [((-1,0),li |! bi), --left
                               ((0,1),ui), --up 
                               ((1,0),li), --right
                               ((0,-1),ui |! bi) -- down 
                              ]
, imgvec = VS.fromList $ map fromIntegral [254,255,255,255,254,254,255,255,255,255,255,255,254,254,254,255,255,254,255,255,1,1,1,255,0,0,0,255,1,1,0,255,254,254,255,255,1,1,1,255,1,1,0,255,0,0,0,255,254,255,255,255,1,1,1,255,0,0,0,255,255,254,255,255]
  }

ur = SimplePattern {
prob = 0.0,
id' = 4,
name = "ur",
allowedNeighbors = M.fromList [ ((-1,0),li |! bi), --left
                               ((0,1),di |! bi), --up 
                               ((1,0),li), --right
                               ((0,-1),di) -- down
                              ]
, imgvec = VS.fromList $ map fromIntegral [255,254,254,255,1,0,1,255,1,1,1,255,254,254,255,255,255,255,254,255,0,1,1,255,0,1,0,255,1,0,1,255,254,255,254,255,0,1,1,255,1,0,0,255,0,1,0,255,255,254,255,255,254,254,255,255,254,254,254,255,255,255,254,255]
  }

bi = V.fromList [1.0,0.0,0.0,0.0,0.0]
dli    = V.fromList [0.0,1.0,0.0,0.0,0.0]
lui    = V.fromList [0.0,0.0,1.0,0.0,0.0]
rdi    = V.fromList [0.0,0.0,0.0,1.0,0.0]
uri    = V.fromList [0.0,0.0,0.0,0.0,1.0]
alli   = V.fromList [1.0,1.0,1.0,1.0,1.0]
li = lui |! dli
ri = rdi |! uri
ui = lui |! uri
di = rdi |! dli
