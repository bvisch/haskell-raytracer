module Raytrace where

import Lib
import Types
import Objects
import Config

import Data.Maybe ( mapMaybe, isJust )
import GHC.Float ( float2Double, double2Float )

import Linear.V4 ( vector )
import Linear.Metric ( Metric(signorm, dot, norm), normalize )
import Linear.Vector ( Additive((^+^), (^-^), zero), (*^), (^*), (^/))
import Linear.Matrix ((!*))

import qualified Graphics.Gloss as G

rayDirection :: World -> G.Point -> Vec4
rayDirection (World time wWidth wHeight camera objs lights) (x,y) = 
        vector $ -1.0 * near *^ (cameraN camera) ^+^ 
        nearWidth * (2.0 * (float2Double x) / (float2Double wWidth) - 1.0) *^ (cameraU camera) ^+^ 
        nearHeight * (2.0 * (float2Double y) / (float2Double wHeight) - 1.0) *^ (cameraV camera)
    where
        near = cameraNear camera
        nearWidth = cameraNearWidth camera
        nearHeight = cameraNearHeight camera


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
        reflectDirOC = normalize $ reflect (-lightDirOC) normalOC
        viewDirOC = normalize $ intersectionOC ^-^ eyeOC
        intersectionWC = objM !* (intersectionOC ^+^ (normalOC ^* epsilon))
        rayToLight = (intersectionWC, (objM !* lightDirOC))

        diffuseIntensity = clampUnit $ lightDirOC `dot` normalOC
        specularIntensity = (** (objF / 4.0)) . clampUnit $ reflectDirOC `dot` viewDirOC
        lightModifier = (lightColour light) * (lightIntensity light) ^/ (lightDistance * lightDistance)
        colour = lightModifier * objAmbRGBA +
                 lightModifier * (diffuseIntensity *^ objDiffRGBA) + 
                 lightModifier * (specularIntensity *^ objSpecRGBA)

reflect :: Vec4 -> Vec4 -> Vec4
reflect incident normal = incident ^-^ (normal ^* (2.0 * incident `dot` normal))

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
                    rayDirOC = normalize $ objMInv !* rayDir
                    intersectionOC = eyeOC ^+^ rayDirOC ^* hitTime
                    normalOC = (objNormal obj) intersectionOC
                    totalLightColour = sum $ mapMaybe (objectColourUnderLight objects obj eyeOC intersectionOC normalOC) lights
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
shade world@(World time wWidth wHeight camera objects lights) point@(x, y) = colourVecToGloss $ trace 0 world ray
    where
        aspect = wWidth / wHeight
        fov = fromIntegral $ configFov defaultConfig
        fovX = fov * aspect
        fovY = fov
        rayDir = normalize $ rayDirection world (x * fovX, y * fovY)
        eye = cameraEye camera
        ray = (eye, rayDir)