module Objects where

import Lib
import Types
import Config

import Linear.V3 ( V3(V3), cross )
import Linear.V4 ( V4(V4) )
import Linear.Matrix ( inv44 )
import Linear.Vector ( Additive((^-^)) )
import Linear.Metric ( Metric(signorm, dot) )


buildCamera :: Point -> Point -> Camera
buildCamera eye g = Camera eye g u v n near nearWidth nearHeight
    where
        up = V3 0.0 0.0 1.0
        n = vec4ToVec3 $ signorm $ eye ^-^ g
        u = signorm $ cross up n
        v = signorm $ cross n u
        near = configNear defaultConfig
        nearWidth = configNearWidth defaultConfig
        nearHeight = configNearHeight defaultConfig

translate :: Float -> Float -> Float -> Mat4
translate x y z = V4 (V4 1.0 0.0 0.0 x) 
                     (V4 0.0 1.0 0.0 y)
                     (V4 0.0 0.0 1.0 z)
                     (V4 0.0 0.0 0.0 1.0)

scale :: Float -> Float -> Float -> Mat4
scale x y z = V4 (V4  x  0.0 0.0 0.0) 
                 (V4 0.0  y  0.0 0.0)
                 (V4 0.0 0.0  z  0.0)
                 (V4 0.0 0.0 0.0 1.0)

-- rotate :: Float -> Float -> Float -> Mat4
-- rotate x y z = (V4 (V4 1.0 0.0 0.0 x) 
--                    (V4 0.0 1.0 0.0 y)
--                    (V4 0.0 0.0 1.0 z)
--                    (V4 0.0 0.0 0.0 1.0)

solveQuadratic :: Float -> Float -> Float -> Maybe Float
solveQuadratic a b c
  | discriminant < 0.0 = Nothing
  | discriminant < epsilon = Just ((-b) / a)
  | min > epsilon = Just min
  | otherwise = Nothing
  where
      discriminant = b * b - a * c
      t1 = (-b) / a - (sqrt discriminant) / a
      t2 = (-b) / a + (sqrt discriminant) / a
      min = if t1 < t2 then t1 else t2

sphereIntersect :: Ray -> Maybe Float
sphereIntersect (eyeH, dirH) = solveQuadratic a b c
    where
        eye = vec4ToVec3 eyeH
        dir = vec4ToVec3 dirH
        a = dir `dot` dir
        b = eye `dot` dir
        c = eye `dot` eye - 1.0

sphereNormal :: Point -> Vec4
sphereNormal = signorm . pointToVec


sphere :: Properties -> Mat4 -> Object
sphere props mat = Sphere sphereIntersect sphereNormal mat (inv44 mat) props


infPlaneIntersect :: Ray -> Maybe Float
infPlaneIntersect (eyeH, dirH)
    | (abs dz) < epsilon = Nothing
    | (-ez)/dz <= 0.0 = Nothing
    | otherwise = Just $ -1.0 * ez / dz
    where
        eye@(V3 ex ey ez) = vec4ToVec3 eyeH
        dir@(V3 dx dy dz) = vec4ToVec3 dirH


infPlaneNormal :: Point -> Vec4
infPlaneNormal _ = V4 0.0 0.0 1.0 0.0

infPlane :: Properties -> Mat4 -> Object
infPlane props mat = InfPlane infPlaneIntersect infPlaneNormal mat (inv44 mat) props