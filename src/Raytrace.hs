module Raytrace where

import Lib
import Types
import Objects
import Config

import System.IO.Unsafe ( unsafePerformIO )

import Data.Maybe ( mapMaybe, isJust )
import GHC.Float ( float2Double, double2Float )
import Data.Bifunctor ( Bifunctor(bimap) )

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
        rayDir = normalize $ vector $ -near *^ (cameraN camera) ^+^ 
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

objectColourUnderLight :: [Object] -> Object -> Ray -> Ray -> Light -> Maybe Colour
objectColourUnderLight objects obj ray@(eyeWC, rayDirWC) reflectedRay@(intersectionWC, reflectDirWC) light = if shadowed objects rayToLight then Nothing else Just colour
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

        intersectionOC = objMInv !* intersectionWC
        normalOC = (objNormal obj) $ intersectionOC
        normalWC = normalize $ objM !* normalOC
        vectorToLightWC = lightPos light ^-^ intersectionWC
        lightDistance = norm vectorToLightWC
        lightDirWC = normalize vectorToLightWC
        reflectDirWC = normalize $ reflect (lightDirWC) normalWC

        viewDirWC = -rayDirWC
        rayToLight = (intersectionWC, lightDirWC)

        diffuseIntensity = clampUnit $ lightDirWC `dot` normalWC
        specularIntensity = (** (objF / 4.0)) . clampUnit $ reflectDirWC `dot` viewDirWC
        lightModifier = (lightColour light) * (lightIntensity light) ^/ (lightDistance * lightDistance)
        colour = lightModifier * objAmbRGBA +
                 lightModifier * (diffuseIntensity *^ objDiffRGBA) + 
                 lightModifier * (specularIntensity *^ objSpecRGBA)

reflect :: Vec4 -> Vec4 -> Vec4
reflect incident normal = incident ^-^ (normal ^* (2.0 * incident `dot` normal))

reflectRay :: Object -> Double -> Ray -> Ray
reflectRay obj hitTime ray@(eyeWC, rayDirWC) = (intersectionWC', reflectDirWC)
    where
        m = objMatInv obj
        mInv = objMatInv obj

        intersectionWC = eyeWC ^+^ rayDirWC ^* hitTime
        intersectionOC = mInv !* intersectionWC
        normalOC = (objNormal obj) intersectionOC

        normalWC = m !* normalOC
        reflectDirWC = normalize $ reflect rayDirWC normalWC
        intersectionWC' = intersectionWC ^+^ normalWC ^* epsilon

trace :: Int -> World -> Ray -> Colour
trace bounces world@(World _ _ _ _ objects lights _ _) ray@(eyeWC, rayDirWC)
    | bounces == configBounces defaultConfig        = zero
    | otherwise                                     =
        let closestObj = closestIntersection ray objects in
        case closestObj of
            Nothing             -> zero
            Just (obj, hitTime) -> 
                let reflectedRay@(intersectionWC, rayDirWC') = reflectRay obj hitTime ray
                    
                    ambCoeff = propAmbCoeff $ objProps obj
                    ambRGBA = propAmbColour $ objProps obj
                    objReflectivity = propReflectivity $ objProps obj
                    totalLightColour = sum $ mapMaybe (objectColourUnderLight objects obj ray reflectedRay) [lights]
                    colourBeforeReflection = (ambCoeff *^ ambRGBA) ^+^ totalLightColour
                    reflectionColour = trace (bounces + 1) world reflectedRay

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
        windowX = (x/2.0 + 0.5) * wWidth
        windowY = (y/2.0 + 0.5) * wHeight
        ray = getFirstRay world (windowX, windowY)


debugRays :: Int -> World -> Ray -> [Ray]
debugRays bounces world@(World _ _ _ camera objects _ _ _) ray@(eye, rayDir)
    | bounces == configBounces defaultConfig = []
    | otherwise                              = 
        let closestObj = closestIntersection ray objects in
        case closestObj of
            Nothing             -> []
            Just (obj, hitTime) -> 
                let objReflectivity = propReflectivity $ objProps obj
                    reflectedRay = reflectRay obj hitTime ray
                    reflections = debugRays (bounces + 1) world reflectedRay
                in
                    if objReflectivity > 0.0 then
                        reflectedRay:reflections
                    else
                        [reflectedRay]

drawDebugRays :: World -> G.Point -> Maybe G.Color
drawDebugRays world@(World time wWidth wHeight camera _ _ _ debugIntersections) point@(x, y) =
    case debugIntersections of
        Just intersections ->
            let threshold = 1.0
                closeTo (x1, y1) (x2, y2) = abs(x2 - x1) < threshold && abs(y2 - y1) < threshold
                windowPoint = ((x/2.0 + 0.5) * wWidth, (y/2.0 + 0.5) * wHeight)
                rayToPixels (eye, dir) = [project eye, project $ eye ^+^ (dir ^* 0.25)]
                project = cameraProject camera
                pointsToDraw = concatMap rayToPixels intersections
            in
                if any (closeTo windowPoint) pointsToDraw then 
                    Just $ colourVecToGloss $ colour 0 255 0 255
                else
                    Nothing
        Nothing -> Nothing


debugAndTrace :: World -> G.Point -> G.Color
debugAndTrace world point =
    case drawDebugRays world point of
        Just colour -> colour
        otherwise   -> shade world point

-- debugRayPoints :: Int -> World -> Ray -> [(Float, Float)]
-- debugRayPoints bounces world@(World _ _ _ camera objects _ _ _) ray@(eye, rayDir)
--     | bounces == configBounces defaultConfig = []
--     | otherwise                              = 
--         let closestObj = closestIntersection ray objects in
--         case closestObj of
--             Nothing             -> []
--             Just (obj, hitTime) -> 
--                 let objReflectivity = propReflectivity $ objProps obj
--                     objM = objMat obj
--                     objMInv = objMatInv obj

--                     eyeOC = objMInv !* eye
--                     rayDirOC = normalize $ objMInv !* rayDir
--                     intersectionOC = eyeOC ^+^ rayDirOC ^* hitTime
--                     normalOC = (objNormal obj) intersectionOC

--                     reflectionVectorOC = reflect intersectionOC normalOC
--                     reflectionVectorWC = normalize $ objM !* reflectionVectorOC
--                     intersectionWC = (objM !* intersectionOC) ^+^ (objM !* normalOC ^* epsilon) -- look here tomorrow
--                     reflections = debugRayPoints (bounces + 1) world (intersectionWC, reflectionVectorWC)

--                     intersectionPixel = cameraProject camera intersectionWC
--                 in
--                     if objReflectivity > 0.0 then
--                         intersectionPixel:reflections
--                     else
--                         [intersectionPixel]

-- debugRay :: World -> G.Point -> Maybe G.Color
-- debugRay world@(World time wWidth wHeight camera _ _ _ debugIntersections) point@(x, y) =
--     case debugIntersections of
--         Just intersections ->
--             let threshold = 1.0
--                 closeTo (x1, y1) (x2, y2) = abs(x2 - x1) < threshold && abs(y2 - y1) < threshold
--                 windowPoint = ((x/2.0 + 0.5) * wWidth, (y/2.0 + 0.5) * wHeight)
--             in
--                 if any (closeTo windowPoint) intersections then 
--                     Just $ colourVecToGloss $ colour 0 255 0 255
--                 else
--                     Nothing
--         Nothing -> Nothing


-- debugAndTrace :: World -> G.Point -> G.Color
-- debugAndTrace world point =
--     case debugRay world point of
--         Just colour -> colour
--         otherwise   -> shade world point