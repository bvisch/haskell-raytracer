module Raytrace where

import Lib
import Types
import Objects
import Config

import System.IO.Unsafe ( unsafePerformIO )

import Data.Maybe ( mapMaybe, isJust )
import GHC.Float ( float2Double, double2Float )

import Linear.V4 ( vector )
import Linear.Metric ( Metric(signorm, dot, norm), normalize )
import Linear.Vector ( Additive((^+^), (^-^), zero), (*^), (^*), (^/))
import Linear.Matrix ((!*))

import qualified Graphics.Gloss as G

getFirstRay :: World -> (Float, Float) -> Ray
getFirstRay (World time wWidth wHeight camera objs lights _ _) (windowX,windowY) = ray
    where
        near = cameraNear camera
        nearWidth = cameraNearWidth camera
        nearHeight = cameraNearHeight camera
        rayDir = normalize $ vector $ -1.0 * near *^ (cameraN camera) ^+^ 
            nearWidth * (2.0 * (float2Double windowX) / (float2Double wWidth) - 1.0) *^ (cameraU camera) ^+^ 
            nearHeight * (2.0 * (float2Double windowY) / (float2Double wHeight) - 1.0) *^ (cameraV camera)
        eye = cameraEye camera
        ray = (eye, rayDir)


getHitTime :: Ray -> Object -> Maybe Double
getHitTime (eyeWC, dirWC) obj = objIntersect obj (eyeOC, dirOC)
    where
        eyeOC = (objMatInv obj) !* eyeWC -- eye and direction in object's coordinate system
        dirOC = (objMatInv obj) !* dirWC

bestHitTime :: (Object, Maybe Double) -> Maybe (Object, Double) -> Maybe (Object, Double)
bestHitTime (o, Nothing) old = old
bestHitTime (o, Just n) old = 
    let new = Just (o, n) in 
        case old of
            Nothing -> new
            Just (o', n') -> if n < n' then new else old

closestIntersection :: Ray -> [Object] -> Maybe (Object, Double)
closestIntersection ray objects = foldr bestHitTime Nothing objectHitTimes
    where
        objectHitTimes = zip objects $ map (getHitTime ray) objects

shadowed :: [Object] -> Ray -> Bool 
shadowed objects ray = any (isJust . getHitTime ray) objects

objectColourUnderLight :: [Object] -> Object -> Point -> Point -> Vec4 -> Light -> Maybe Colour
objectColourUnderLight objects obj eyeOC intersectionOC normalOC light = if shadowed objects rayToLight then Nothing else Just colour
-- objectColourUnderLight objects obj eyeOC intersectionOC normalOC light = Just colour
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
        objMInv = objMatInv obj
        
        vectorToLightOC = (objMInv !* lightPos light) ^-^ intersectionOC
        lightDistance = norm vectorToLightOC
        lightDirOC = normalize vectorToLightOC
        reflectDirOC = normalize $ reflect (lightDirOC) normalOC
        viewDirOC = normalize $ intersectionOC ^-^ eyeOC
        intersectionWC = objM !* (intersectionOC ^+^ (normalOC ^* epsilon))
        rayToLight = (intersectionWC, (normalize $ objM !* lightDirOC))

        diffuseIntensity = clampUnit $ lightDirOC `dot` normalOC
        specularIntensity = (** (objF / 4.0)) . clampUnit $ reflectDirOC `dot` viewDirOC
        lightModifier = (lightColour light) * (lightIntensity light) ^/ (lightDistance * lightDistance)
        colour = lightModifier * objAmbRGBA +
                 lightModifier * (diffuseIntensity *^ objDiffRGBA) + 
                 lightModifier * (specularIntensity *^ objSpecRGBA)

reflect :: Vec4 -> Vec4 -> Vec4
reflect incident normal = incident ^-^ (normal ^* (2.0 * incident `dot` normal))

trace :: Int -> World -> Ray -> Colour
trace bounces world@(World _ _ _ _ objects lights _ _) ray@(eye, rayDir)
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
                    rayDirOC = normalize $ objMInv !* rayDir
                    intersectionOC = eyeOC ^+^ rayDirOC ^* hitTime
                    normalOC = (objNormal obj) intersectionOC
                    totalLightColour = sum $ mapMaybe (objectColourUnderLight objects obj eyeOC intersectionOC normalOC) [lights]
                    colourBeforeReflection = (ambCoeff *^ ambRGBA) ^+^ totalLightColour

                    reflectionVectorOC = reflect intersectionOC normalOC
                    reflectionVectorWC = normalize $ objM !* reflectionVectorOC
                    intersectionWC = (objM !* intersectionOC) ^+^ (objM !* normalOC ^* epsilon) -- look here tomorrow
                    reflectionColour = trace (bounces + 1) world (intersectionWC, reflectionVectorWC)

                    colour = 
                        if objReflectivity > 0.0 then
                            ((1.0 - objReflectivity) *^ colourBeforeReflection) ^+^ (objReflectivity *^ reflectionColour)
                        else
                            colourBeforeReflection

                    -- colour = colourBeforeReflection
                in colour

shade :: World -> G.Point -> G.Color
shade world@(World _ wWidth wHeight _ _ _ _ _) point@(x, y) = colourVecToGloss $ trace 0 world ray
    where
        aspect = wWidth / wHeight
        fov = fromIntegral $ configFov defaultConfig
        windowX = x * wWidth + wWidth / 2
        windowY = y * wHeight + wHeight / 2
        ray = getFirstRay world (windowX, windowY)


debugRayPoints :: Int -> World -> Ray -> [(Float, Float)]
debugRayPoints bounces world@(World _ _ _ camera objects _ _ _) ray@(eye, rayDir)
    | bounces == configBounces defaultConfig = []
    | otherwise                              = 
        let closestObj = closestIntersection ray objects in
        case closestObj of
            Nothing             -> []
            Just (obj, hitTime) -> 
                let objReflectivity = propReflectivity $ objProps obj
                    objM = objMat obj
                    objMInv = objMatInv obj

                    eyeOC = objMInv !* eye
                    rayDirOC = normalize $ objMInv !* rayDir
                    intersectionOC = eyeOC ^+^ rayDirOC ^* hitTime
                    normalOC = (objNormal obj) intersectionOC

                    reflectionVectorOC = reflect intersectionOC normalOC
                    reflectionVectorWC = normalize $ objM !* reflectionVectorOC
                    intersectionWC = (objM !* intersectionOC) ^+^ (objM !* normalOC ^* epsilon) -- look here tomorrow
                    reflections = debugRayPoints (bounces + 1) world (intersectionWC, reflectionVectorWC)

                    intersectionPixel = cameraProject camera intersectionWC
                in
                    if objReflectivity > 0.0 then
                        intersectionPixel:reflections
                    else
                        [intersectionPixel]

-- debugRay :: World -> G.Point -> Maybe G.Color
-- debugRay world@(World time wWidth wHeight camera objects lights clickPos _) point@(x, y) =
--     case clickPos of
--         Just pos@(posX, posY) -> 
--             let 
--                 threshold = 0.01
--                 closeTo (x1, y1) (x2, y2) = abs(x2 - x1) < threshold && abs(y2 - y1) < threshold
--                 windowPoint = (x * wWidth + wWidth / 2.0, y * wHeight + wHeight / 2.0)
--                 actualPos = (posX + wWidth / 2.0, posY + wHeight / 2.0)
--                 ray = getFirstRay world actualPos
--                 intersections = debugRayPoints 0 world ray
--             in
--                 unsafePerformIO $ do
--                     putStrLn (show intersections)
--                     return $
--                         if any (closeTo windowPoint) intersections then 
--                             Just $ colourVecToGloss $ colour 0 255 0 255
--                         else
--                             Nothing
--         Nothing -> Nothing

debugRay :: World -> G.Point -> Maybe G.Color
debugRay world@(World time wWidth wHeight camera _ _ _ debugIntersections) point@(x, y) =
    case debugIntersections of
        Just intersections ->
            let threshold = 1.0
                closeTo (x1, y1) (x2, y2) = abs(x2 - x1) < threshold && abs(y2 - y1) < threshold
                windowPoint = (x * wWidth + wWidth / 2.0, y * wHeight + wHeight / 2.0)
            in
                if any (closeTo windowPoint) intersections then 
                    Just $ colourVecToGloss $ colour 0 255 0 255
                else
                    Nothing
        Nothing -> Nothing


debugAndTrace :: World -> G.Point -> G.Color
debugAndTrace world point =
    case debugRay world point of
        Just colour -> colour
        otherwise   -> shade world point