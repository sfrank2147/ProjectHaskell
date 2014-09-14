--use memoization to store smaller paths

import qualified Data.Map as Map
import Data.Maybe

main = putStrLn . show $ fromJust $ Map.lookup (20,20) pathCountsMap

pathCountsMap = Map.fromList [((x,y), pathCounts x y) | x <- [0..20], y <- [0..20]]
    where pathCounts 0 _ = 1
          pathCounts _ 0 = 1
          pathCounts w h = (fromJust $ Map.lookup ((w-1),h) pathCountsMap) + (fromJust $ Map.lookup (w, (h-1)) pathCountsMap)