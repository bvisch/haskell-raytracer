module Lib where

import Linear.V3 ( V3(..) )
import Linear.V4 ( V4(..) )
import Linear.Matrix ( M44 )

import qualified Graphics.Gloss                         as G

import Data.Bifunctor (second)

type Point = V4 Float -- point in homogeneous coordinates
type Vec4 = V4 Float
type Vec3 = V3 Float
type Mat4 = M44 Float
type Colour = Vec4
type Ray = (Point, Vec4)

mapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSnd f = map (second f)

clamp :: Float -> Float -> Float -> Float
clamp min max x
  | x > max = max
  | x < min = min
  | otherwise = x

colourVecToGloss :: Colour -> G.Color
colourVecToGloss (V4 r g b a) = G.makeColorI (floor r) (floor g) (floor b) (floor a)

pointToVec :: Point -> Vec4
pointToVec (V4 x y z w) = V4 x y z 0.0

vecToPoint :: Vec4 -> Point
vecToPoint (V4 x y z w) = V4 x y z 1.0

vec4ToVec3 :: Vec4 -> Vec3
vec4ToVec3 (V4 x y z w) = V3 x y z

colour :: Int -> Int -> Int -> Int -> Colour
colour r g b a = V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

epsilon :: Float
epsilon = 0.00001

maxIntensity :: Float
maxIntensity = 254.0

clampUnit :: Float -> Float
clampUnit = clamp 0.0 (1.0 - epsilon)