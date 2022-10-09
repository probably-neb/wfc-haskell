module Utils where

-- UTIL TYPES --
type RwCl = (Int, Int)
type WdHt = (Int, Int)
type Hash = Int


mapTuple :: ( a -> b ) -> (a,a) -> (b,b)
mapTuple f (a1,a2) = ( f a1, f a2 )

ignoreFirstArg :: (x -> y -> z) -> k -> x -> y -> z
ignoreFirstArg f _ x y = f x y

tozip :: (a -> b) -> (a -> c) -> [a] -> [(b,c)]
tozip fb fc = map (\a -> (fb a, fc a))

zipUsing :: (a -> c) -> (b -> d) -> [a] -> [b] -> [(c,d)]
zipUsing fa fb = zipWith (\a b -> (fa a, fb b))

bigzip :: (Foldable f) => (a -> f d -> f d) -> (b -> f e -> f e) -> (f d,f e) -> f (a,b) -> (f d, f e)
bigzip ca cb de0 abs  = foldl (\(sa,sb) (a,b) -> (ca a sa, cb b sb)) de0 abs

filterE :: Eq e => e -> (a -> e) -> [a] -> [a]
filterE e fe as = filter (\a -> fe a == e) as

filterNE :: Eq e => e -> (a -> e) -> [a] -> [a]
filterNE e fe as = filter (\a -> fe a /= e) as

dup :: a -> (a,a)
dup a = (a,a)
