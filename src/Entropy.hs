module Entropy (entropy) where
import Data.Foldable (foldl')
-- calculates shannon entropy from list of probabilities
--
entropyStep :: Double -> Double
entropyStep prob | prob <= 0.0 = 0.0
                 | otherwise = (prob * logBase 2 (1/prob))

entropy :: Foldable f => f Double -> Double
-- entropy probabilities = sum [p * logBase 2 (1/p) | p <- toList probabilities]
entropy probabilities = foldl' (\sum' prob -> sum' + entropyStep prob) 0.0 probabilities

-- entropy' :: [Double] -> Double
-- entropy' probabilities = sum [p * logBase 2 (1/p) | p <- probabilities]

-- testEntropy :: IO ()
-- testEntropy = let probs = map (/ 10) [1..8] in
--   print ( entropy probs )
