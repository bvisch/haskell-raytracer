module Types where

import Lib ( Colour, Mat4, Point, Ray, Vec3, Vec4 )

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


data Camera = Camera { cameraEye :: Point,
                       cameraG :: Point,
                       cameraU :: Vec3,
                       cameraV :: Vec3,
                       cameraN :: Vec3,
                       cameraNear :: Float,
                       cameraNearWidth :: Float,
                       cameraNearHeight :: Float }


data World = World { worldTime :: Float,
                     worldWindowWidth :: Float,
                     worldWindowHeight :: Float,
                     worldCamera :: Camera,
                     worldObjects :: [Object],
                     worldLights :: [Light] }