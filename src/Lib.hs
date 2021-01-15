module Lib
    ( mapSnd
    ) where

import Data.Bifunctor (second)

mapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSnd f = map (second f)
