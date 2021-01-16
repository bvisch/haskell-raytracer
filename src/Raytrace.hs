module Raytrace where

import Lib
import Types
import Objects
import Config

import Data.Maybe ( mapMaybe, isJust )

import Linear.V4 ( vector )
import Linear.Metric ( Metric(signorm, dot) )
import Linear.Vector ( Additive((^+^), (^-^), zero), (*^), (^*) )
import Linear.Matrix ((!*))

import qualified Graphics.Gloss as G

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

shadowed :: [Object] -> Ray -> Bool 
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
        intersectionWC = (objM !* intersectionOC) ^+^ (normal ^* epsilon)
        rayToLight = (intersectionWC, (objM !* vectorToLight))
        lightModifier = (lightColour light) * (lightIntensity light)
        colour = lightModifier * ((diffuseIntensity *^ objDiffRGBA) + (specularIntensity *^ objSpecRGBA))

reflect :: Vec4 -> Vec4 -> Vec4
reflect incident normal = (normal ^* (2.0 * incident `dot` normal)) ^-^ incident

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
                    colourBeforeReflection = (ambCoeff *^ ambRGBA) ^+^ totalLightColour

                    reflectionVector = reflect intersectionOC normal
                    reflectionVectorWC = signorm $ objM !* reflectionVector
                    intersectionWC = (objM !* intersectionOC) ^+^ (reflectionVectorWC ^* epsilon)
                    reflectionColour = trace (bounces + 1) world (intersectionWC, reflectionVectorWC)

                    colour = 
                        if objReflectivity > 0.0 then
                            ((1.0 - objReflectivity) *^ colourBeforeReflection) ^+^ (objReflectivity *^ reflectionColour)
                        else
                            colourBeforeReflection

                    -- colour = colourBeforeReflection
                in colour


shade :: World -> G.Point -> G.Color
shade world@(World time wWidth wHeight camera objects lights) point@(x, y) = colourVecToGloss $ trace 0 world ray
    where
        aspect = wWidth / wHeight
        fov = fromIntegral $ configFov defaultConfig
        fovX = fov * aspect
        fovY = fov
        rayDir = rayDirection world (x * fovX, y * fovY)
        eye = cameraEye camera
        ray = (eye, rayDir)