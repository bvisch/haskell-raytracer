module Lib where

import Linear.V3 ( V3(..) )
import Linear.V4 ( V4(..) )
import Linear.Matrix ( M44 )

import qualified Graphics.Gloss                         as G

import Data.Bifunctor (second)

type Point = V4 Double -- point in homogeneous coordinates
type Vec4 = V4 Double
type Vec3 = V3 Double
type Mat4 = M44 Double
type Colour = Vec4
type Ray = (Point, Vec4)

mapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSnd f = map (second f)

clamp :: Double -> Double -> Double -> Double
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

epsilon :: Double
epsilon = 0.00001

maxIntensity :: Double
maxIntensity = 254.0

clampUnit :: Double -> Double
clampUnit = clamp 0.0 (1.0 - epsilon)