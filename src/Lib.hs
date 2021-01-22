module Lib where

-- import qualified Linear.V3 as L
-- import qualified Linear.V4 as L
-- import qualified Linear.Matrix as L

import Linear.V3
import Linear.V4
import Linear.Matrix

import qualified Graphics.Gloss as G

import Data.Bifunctor (second)

-- newtype Point = Point { unpoint :: V4 Double } -- point in homogeneous coordinates
-- newtype UnitV = UnitV { ununit :: V4 Double }

type Point = V4 Double
type Vec4 = V4 Double
type Vec3 = V3 Double
type Mat4 = M44 Double
type Colour = Vec4
type Ray = (Point, Vec4)
type Pixel = ((Float, Float), Colour)



-- f :: (V4 Double -> V4 Double) -> (Point -> Point)
-- f g = Point . g . unpoint

-- f2 :: (V4 Double -> V4 Double -> V4 Double) -> (Point -> Point -> Point)
-- f2 o p1 p2 = Point $ (unpoint p1) `o` (unpoint p2)

-- (^-^) = f2 L.(^-^)

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

red :: Colour
red = colour 255 0 0 255

green :: Colour
green = colour 0 255 0 255

blue :: Colour
blue = colour 0 0 255 255

magenta :: Colour
magenta = colour 255 0 255 255

cyan :: Colour
cyan = colour 0 255 255 255

yellow :: Colour
yellow = colour 255 255 0 255

white :: Colour
white = colour 255 255 255 255

epsilon :: Double
epsilon = 0.0001

maxIntensity :: Double
maxIntensity = 254.0

clampUnit :: Double -> Double
clampUnit = clamp 0.0 (1.0 - epsilon)