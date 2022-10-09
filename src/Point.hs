module Point (Point,
   add,
   subtract,
   normalize,
   getDirection,
   inbounds,
   within,
   unsafeAdjacents,
   adjacents,
   adjacentDirs,
   getXPatCoords,
   getYPatCoords,
   getPatCoords,
   getCoords,
   getRwCls,
   ncardinalDirs,
   cardinalDirs,
   nDir,
   unDir,
   rPoint,
   ddTodIdx,
   toFloat
) where 

import Utils
import Prelude hiding (subtract)
import Data.List.Split (chunksOf)


type Point = (Int, Int) 
type Dir = Point

pointEval :: (Int -> Int -> Int) -> Point -> Point -> Point
pointEval f (a,b) (x,y) = (f a x, f b y)

add :: Point -> Point -> Point
add = pointEval (+)

toFloat :: Point -> (Float,Float)
toFloat = mapTuple fromIntegral

subtract :: Point -> Point -> Point
subtract = pointEval (-)

normalizeHelper :: Int -> Int
normalizeHelper n | n > 0     = div n n
                  | n < 0     = div n (abs n)
                  | otherwise = 0

normalize :: Point -> Point
normalize p = mapTuple normalizeHelper p

within :: Point -> WdHt -> Bool
within (x,y) (xbound,ybound) = x >= 0 && y >= 0 && x < xbound && y < ybound

rPoint :: Dir -> Point -> Int -> Point
rPoint d p n = add (nDir n d) p

-- like within but bounds are interpreted as rows and columns
inbounds :: Int -> RwCl -> Point -> Bool
inbounds n (rows,cols) xy = within xy (cols * n, rows * n)

getRwCls :: WdHt -> Int -> RwCl
getRwCls wdHt n = mapTuple divn wdht
  where divn = (`div` n) -- curry div
        wdht = wdHt

getYPatCoords :: RwCl -> Int -> [Int]
getYPatCoords (rows,cols) n = concatMap (replicate cols . (*n)) [0..rows-1]

getXPatCoords :: RwCl -> Int -> [Int]
getXPatCoords (rows,cols) n = (concat . concat) (replicate rows (chunksOf n (map (* n) [0 .. cols-1])))

getPatCoords :: RwCl -> Int -> [Point]
getPatCoords rowcol n= zip (getXPatCoords rowcol n) (getYPatCoords rowcol n)

-- coordsHelper :: Int -> Int -> [Int]
-- coordsHelper dim1 dim2 = concat $ replicate dim2 [0..dim1-1]

getYCoords :: WdHt -> [Int]
getYCoords (w,h) = concatMap (replicate w) [0..h-1]

getXCoords :: WdHt -> [Int]
getXCoords (w,h) = concat $ replicate h [0..w-1]

getCoords :: WdHt -> [(Int,Int)]
getCoords wh = zip (getXCoords wh) (getYCoords wh)

---------------------------------------------------------------------

-- multiplies a dir by n (0,1) -> (0,n)
nDir :: Int -> Point -> Point
nDir n dir = mapTuple (*n) dir

-- the opposite of nDir
unDir :: Point -> Point
unDir ndir = normalize ndir

cardinalDirs :: [Point] -- not using 4 tuple for stronger type definition to avoid annoyance mapping which I plan to do often
cardinalDirs = [(0,1),(0,-1),(1,0),(-1,0)]

ncardinalDirs :: Int -> [Point]
ncardinalDirs n = map (nDir n) cardinalDirs

unsafeAdjacents :: Int -> Point -> [Point]
unsafeAdjacents n p = map (add p) (ncardinalDirs n)

-- returns the inbounds patterns/tiles around a point
adjacents :: Int -> RwCl -> Point -> [Point]
adjacents n rwCl p = filter (inbounds n rwCl) (map (add p) (ncardinalDirs n)) -- r l u d

-- returns the directions to inbounds neighbors
adjacentDirs :: Int -> RwCl -> Point -> [Point]
adjacentDirs n rwCl p = map (\np -> getDirection np p) (adjacents n rwCl p)
-- takes point and its neighbor and returns the direction the neighbor is in

getDirection :: Point -> Point -> Point
getDirection to from = normalize (subtract to from)

ddTodIdx :: Int -> Int -> Int -> Int
ddTodIdx w r c = w * r + c
-- 
