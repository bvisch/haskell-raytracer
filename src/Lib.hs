module Lib
    ( mapSnd,
      clamp
    ) where

import Data.Bifunctor (second)

mapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSnd f = map (second f)

clamp :: Float -> Float -> Float -> Float
clamp min max x
  | x > max = max
  | x < min = min
  | otherwise = x