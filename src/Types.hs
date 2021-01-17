module Types where

import Lib ( Colour, Mat4, Point, Ray, Vec3, Vec4 )

data Properties = Properties { propDensity :: Double,
                               propReflectivity :: Double,
                               propSpecCoeff :: Double,
                               propDiffCoeff :: Double,
                               propAmbCoeff :: Double,
                               propSpecColour :: Colour,
                               propDiffColour :: Colour,
                               propAmbColour :: Colour,
                               propF :: Double }

data Object = Sphere   { objIntersect :: Ray -> Maybe Double, 
                         objNormal :: Point -> Vec4,
                         objMat :: Mat4,
                         objMatInv :: Mat4,
                         objProps :: Properties }
            | InfPlane { objIntersect :: Ray -> Maybe Double,
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
                       cameraNear :: Double,
                       cameraNearWidth :: Double,
                       cameraNearHeight :: Double }


data World = World { worldTime :: Double,
                     worldWindowWidth :: Float,
                     worldWindowHeight :: Float,
                     worldCamera :: Camera,
                     worldObjects :: [Object],
                     worldLights :: [Light] }