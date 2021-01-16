module Main where

import Lib
import System.IO.Unsafe

import qualified Graphics.Gloss                         as G
import qualified Graphics.Gloss.Interface.Pure.Game     as G
import qualified Graphics.Gloss.Raster.Field            as G
import Linear.V3
import Linear.V4
import Linear.Matrix (M44, (!*), identity, inv44)
import Linear.Vector
import Linear.Metric

import Control.Monad
import Data.Maybe
import Data.Bifunctor (second, bimap)

type Point = V4 Float -- point in homogeneous coordinates
type Vec4 = V4 Float
type Vec3 = V3 Float
type Mat4 = M44 Float
type Colour = Vec4
type Ray = (Point, Vec4)

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

data Config = Config { configNear :: Float,
                       configFar :: Float,
                       configNearWidth :: Float,
                       configNearHeight :: Float,
                       configWindowWidth :: Int, 
                       configWindowHeight :: Int,
                       configFov :: Int,
                       configStepsPerSecond :: Int, 
                       configBounces :: Int, 
                       configZoom :: Int }

defaultConfig :: Config
defaultConfig = Config { configNear = near,
                         configFar = 25.0,
                         configNearWidth = width,
                         configNearHeight = height,
                         configWindowWidth = 600, 
                         configWindowHeight = 400,
                         configFov = 460,
                         configStepsPerSecond = 10, 
                         configBounces = 3, 
                         configZoom = 1 }
    where
        near = 1.0
        theta = 45.0
        aspect = 1.5
        height = near * tan(pi / 180.0 * theta / 2.0)
        width = height * aspect

data Camera = Camera { cameraEye :: Point,
                       cameraG :: Point,
                       cameraU :: Vec3,
                       cameraV :: Vec3,
                       cameraN :: Vec3,
                       cameraNear :: Float,
                       cameraNearWidth :: Float,
                       cameraNearHeight :: Float }

data Properties = Properties { propDensity :: Float,
                               propReflectivity :: Float,
                               propSpecCoeff :: Float,
                               propDiffCoeff :: Float,
                               propAmbCoeff :: Float,
                               propSpecColour :: Colour,
                               propDiffColour :: Colour,
                               propAmbColour :: Colour,
                               propF :: Float }

data Object = Sphere   { objIntersect :: Ray -> Maybe Float, 
                         objNormal :: Point -> Vec4,
                         objMat :: Mat4,
                         objMatInv :: Mat4,
                         objProps :: Properties }
            | InfPlane { objIntersect :: Ray -> Maybe Float,
                         objNormal :: Point -> Vec4,
                         objMat :: Mat4,
                         objMatInv :: Mat4,
                         objProps :: Properties }

data Light = Light { lightPos :: Point,
                     lightColour :: Colour,
                     lightIntensity :: Colour }

data World = World { worldTime :: Float,
                     worldWindowWidth :: Float,
                     worldWindowHeight :: Float,
                     worldCamera :: Camera,
                     worldObjects :: [Object],
                     worldLights :: [Light] }

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



lookAt :: Camera -> Vec4 -> Camera
lookAt = undefined

initialWorld :: World
initialWorld = World { worldTime = 0.0,
                       worldWindowWidth = fromIntegral $ configWindowWidth defaultConfig,
                       worldWindowHeight = fromIntegral $ configWindowHeight defaultConfig,
                       worldCamera = buildCamera eye g,
                       worldObjects = objects,
                       worldLights = lights }
    where
        eye = V4 1.0 2.0 1.0 1.0
        g = V4 0.0 0.0 0.0 1.0
        colour1 = colour 125 25 255 255
        colour2 = colour 0 0 255 255
        sphere1Props = Properties 1.0 0.5 0.6 0.2 0.2 colour1 colour1 colour1 10.0
        planeProps = Properties 1.0 1.0 0.6 0.6 0.6 colour2 colour2 colour2 10.0
        objects = [sphere sphere1Props (translate 0.0 0.0 0.0),
                   infPlane planeProps identity]
        lights = [Light (V4 10.0 5.0 5.0 1.0) (colour 255 255 255 255) (colour 255 255 255 255)]

handleEvent :: G.Event -> World -> World
handleEvent event world
    | G.EventKey (G.Char 'w') G.Down _ _        <- event
        = world { worldCamera = buildCamera (V4 ex (ey + 0.1) ez ew) g }

    | G.EventKey (G.Char 'a') G.Down _ _        <- event
        = world { worldCamera = buildCamera (V4 (ex - 0.1) ey ez ew) g }

    | G.EventKey (G.Char 's') G.Down _ _        <- event
        = world { worldCamera = buildCamera (V4 ex (ey - 0.1) ez ew) g }

    | G.EventKey (G.Char 'd') G.Down _ _        <- event
        = world { worldCamera = buildCamera (V4 (ex + 0.1) ey ez ew) g }

    | G.EventKey (G.Char 'r') G.Down _ _        <- event
        = world { worldCamera = buildCamera (V4 ex ey (ez + 0.1) ew) g }

    | G.EventKey (G.Char 'f') G.Down _ _        <- event
        = world { worldCamera = buildCamera (V4 ex ey (ez - 0.1) ew) g }

    | G.EventKey (G.Char 'u') G.Down _ _        <- event
        = world { worldCamera = buildCamera eye (V4 gx (gy + 0.1) gz gw) }

    | G.EventKey (G.Char 'h') G.Down _ _        <- event
        = world { worldCamera = buildCamera eye (V4 (gx - 0.1) gy gz gw) }

    | G.EventKey (G.Char 'j') G.Down _ _        <- event
        = world { worldCamera = buildCamera eye (V4 gx (gy - 0.1) gz gw) }

    | G.EventKey (G.Char 'k') G.Down _ _        <- event
        = world { worldCamera = buildCamera eye (V4 (gx + 0.1) gy gz gw) }

    | G.EventKey (G.Char 'o') G.Down _ _        <- event
        = world { worldCamera = buildCamera eye (V4 gx gy (gz + 0.1) gw) }

    | G.EventKey (G.Char 'l') G.Down _ _        <- event
        = world { worldCamera = buildCamera eye (V4 gx gy (gz - 0.1) gw) }

    | otherwise = world
    where
        camera@(Camera eye@(V4 ex ey ez ew) g@(V4 gx gy gz gw) _ _ _ _ _ _) = worldCamera world

advanceWorld :: Float -> World -> World
advanceWorld time world = world

rayDirection :: World -> G.Point -> Vec4
rayDirection (World time wWidth wHeight camera objs lights) (x,y) = 
        vector $ -1.0 * near *^ (cameraN camera) ^+^ 
        nearWidth * (2.0 * x / wWidth - 1.0) *^ (cameraU camera) ^+^ 
        nearHeight * (2.0 * y / wHeight - 1.0) *^ (cameraV camera)
    where
        near = cameraNear camera
        nearWidth = cameraNearWidth camera
        nearHeight = cameraNearHeight camera


getHitTime :: Ray -> Object -> Maybe Float
getHitTime (eyeWC, dirWC) obj = objIntersect obj (eyeOC, dirOC)
    where
        eyeOC = (objMatInv obj) !* eyeWC -- eye and direction in object's coordinate system
        dirOC = (objMatInv obj) !* dirWC

bestHitTime :: (Object, Maybe Float) -> Maybe (Object, Float) -> Maybe (Object, Float)
bestHitTime (o, Nothing) old = old
bestHitTime (o, Just n) old = 
    let new = Just (o, n) in 
        case old of
            Nothing -> new
            Just (o', n') -> if n < n' then new else old

closestIntersection :: Ray -> [Object] -> Maybe (Object, Float)
closestIntersection ray objects = foldr bestHitTime Nothing objectHitTimes
    where
        objectHitTimes = zip objects $ map (getHitTime ray) objects

shadowed :: [Object] -> Ray -> Bool  -- O(n^2) when mapped unless ghc has some magic
shadowed objects ray = any (isJust . getHitTime ray) objects

objectColourUnderLight :: [Object] -> Object -> Point -> Point -> Vec4 -> Light -> Maybe Colour
objectColourUnderLight objects obj eyeOC intersectionOC normal light = if shadowed objects rayToLight then Nothing else Just colour
    where
        (Properties objDens 
                    objRefl
                    specCoeff
                    diffCoeff
                    ambCoeff
                    objSpecRGBA
                    objDiffRGBA
                    objAmbRGBA
                    objF) = objProps obj

        objM = objMat obj
        
        vectorToLight = signorm $ (lightPos light) ^-^ intersectionOC
        specularReflection = signorm $ (vectorToLight ^* (-1.0)) ^+^ (normal ^* (2.0 * normal `dot` vectorToLight))
        vectorToCenterOfProjection = signorm $ intersectionOC ^-^ eyeOC
        diffuseIntensity = clampUnit . dot normal $ vectorToLight
        specularIntensity = (** objF) . clampUnit $ specularReflection `dot` vectorToCenterOfProjection
        intersectionWC = (objM !* intersectionOC) ^+^ V4 0.0 0.0 epsilon 0.0
        rayToLight = (intersectionWC, (objM !* vectorToLight))
        lightModifier = (lightColour light) * (lightIntensity light)
        colour = lightModifier * ((diffuseIntensity *^ objDiffRGBA) + (specularIntensity *^ objSpecRGBA))

reflect :: Vec4 -> Vec4 -> Vec4
reflect incident normal = (normal *^ (2.0 * incident `dot` normal)) ^-^ incident

trace :: Int -> World -> Ray -> Colour
trace bounces world@(World _ _ _ _ objects lights) ray@(eye, rayDir)
    | bounces == configBounces defaultConfig        = zero
    | otherwise                                     =
        let closestObj = closestIntersection ray objects in
        case closestObj of
            Nothing             -> zero
            Just (obj, hitTime) -> 
                let ambCoeff = propAmbCoeff $ objProps obj
                    ambRGBA = propAmbColour $ objProps obj
                    objReflectivity = propReflectivity $ objProps obj
                    objM = objMat obj
                    objMInv = objMatInv obj

                    eyeOC = objMInv !* eye
                    rayDirOC = objMInv !* rayDir
                    intersectionOC = eyeOC ^+^ rayDirOC ^* hitTime
                    normal = (objNormal obj) intersectionOC
                    totalLightColour = sum $ mapMaybe (objectColourUnderLight objects obj eyeOC intersectionOC normal) lights
                    colourBeforeReflection = (ambCoeff *^ ambRGBA)

                    reflectionVector = reflect intersectionOC normal
                    reflectionVectorWC = objM !* reflectionVector
                    intersectionWC = (objM !* intersectionOC) ^+^ (reflectionVector ^* epsilon)
                    reflectionColour = trace (bounces + 1) world (intersectionWC, reflectionVectorWC)

                    colour = 
                        if objReflectivity > 0.0 then
                            ((1.0 - objReflectivity) *^ colourBeforeReflection) ^+^ (objReflectivity *^ reflectionColour)
                        else
                            colourBeforeReflection
                in colour

-- shade :: World -> G.Point -> G.Color
-- shade world point = colourVecToGloss $ trace 0 (cameraEye camera) world point
--     where

shade :: World -> G.Point -> G.Color
shade world point = colourVecToGloss $ trace 0 world point
        
        

main :: IO ()
main = G.playField
        (G.InWindow "Raytracer" (configWindowWidth defaultConfig, configWindowHeight defaultConfig) (10, 10))
        zoom
        (configStepsPerSecond defaultConfig)
        initialWorld
        shade
        handleEvent
        advanceWorld
    where
        zoom = (configZoom defaultConfig, configZoom defaultConfig)