module Objects where

import Lib
import Types
import Config

import GHC.Float ( double2Float )

import Control.Lens

import Linear.V3 ( V3(V3), cross )
import Linear.V4 ( V4(V4), _xyz, _w )
import Linear.Matrix ( (!*), (!*!), inv44 )
import Linear.Vector ( Additive((^-^)), (^/) )
import Linear.Metric ( Metric(dot), normalize )
import Linear.Projection

buildCamera :: Point -> Point -> Camera
buildCamera eye g = Camera eye g u v n near nearWidth nearHeight viewMat frustumMat screenMat cameraMat
    where
        (Config near far nearWidth nearHeight windowWidth windowHeight viewAngle aspectRatio _ _ _ _) = defaultConfig 
        up = V3 0.0 0.0 1.0
        n = vec4ToVec3 $ normalize $ eye ^-^ g
        u = normalize $ cross up n
        v = normalize $ cross n u

        viewMat = lookAt (eye ^. _xyz) (g ^. _xyz) up
        theta = viewAngle
        t = near * tan (pi / 180.0 * theta / 2)
        b = -t
        r = aspectRatio * t
        l = -r
        frustumMat = frustum l r b t near far
        w = (fromIntegral windowWidth) / 2.0
        h = (fromIntegral windowHeight) / 2.0
        screenMat = V4 (V4  w  0.0 0.0 1.0)
                       (V4 0.0  h  0.0 1.0)
                       (V4 0.0 0.0 1.0 0.0)
                       (V4 0.0 0.0 0.0 1.0)

        cameraMat = screenMat !*! frustumMat !*! viewMat

cameraProject :: Camera -> Point -> (Float, Float)
cameraProject camera point = (double2Float x, double2Float y)
    where
        pointTransformed = (cameraMat camera) !* point
        pointPerspProj@(V4 x y _ _) = pointTransformed ^/ (pointTransformed ^. _w)

translate :: Double -> Double -> Double -> Mat4
translate x y z = V4 (V4 1.0 0.0 0.0 x) 
                     (V4 0.0 1.0 0.0 y)
                     (V4 0.0 0.0 1.0 z)
                     (V4 0.0 0.0 0.0 1.0)

scale :: Double -> Double -> Double -> Mat4
scale x y z = V4 (V4  x  0.0 0.0 0.0) 
                 (V4 0.0  y  0.0 0.0)
                 (V4 0.0 0.0  z  0.0)
                 (V4 0.0 0.0 0.0 1.0)

-- rotate :: Double -> Double -> Double -> Mat4
-- rotate x y z = (V4 (V4 1.0 0.0 0.0 x) 
--                    (V4 0.0 1.0 0.0 y)
--                    (V4 0.0 0.0 1.0 z)
--                    (V4 0.0 0.0 0.0 1.0)

solveQuadratic :: Double -> Double -> Double -> Maybe Double
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

sphereIntersect :: Ray -> Maybe Double
sphereIntersect (eyeH, dirH) = solveQuadratic a b c
    where
        eye = vec4ToVec3 eyeH
        dir = vec4ToVec3 dirH
        a = dir `dot` dir
        b = eye `dot` dir
        c = eye `dot` eye - 1.0

sphereNormal :: Point -> Vec4
sphereNormal = normalize . pointToVec


sphere :: Properties -> Mat4 -> Object
sphere props mat = Sphere sphereIntersect sphereNormal mat (inv44 mat) props


infPlaneIntersect :: Ray -> Maybe Double
infPlaneIntersect (eye@(V4 ex ey ez ew), dir@(V4 dx dy dz dw))
    | (abs dz) < epsilon = Nothing
    | (-ez)/dz <= 0.0 = Nothing
    | otherwise = Just $ -1.0 * ez / dz


infPlaneNormal :: Point -> Vec4
infPlaneNormal _ = V4 0.0 0.0 1.0 0.0

infPlane :: Properties -> Mat4 -> Object
infPlane props mat = InfPlane infPlaneIntersect infPlaneNormal mat (inv44 mat) props