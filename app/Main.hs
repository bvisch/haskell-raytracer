module Main where

import Lib
import Types
import Raytrace
import Config
import Objects
import System.IO.Unsafe

import qualified Graphics.Gloss                         as G
import qualified Graphics.Gloss.Interface.Pure.Game     as G
import qualified Graphics.Gloss.Raster.Field            as G

import Linear.V4
import Linear.Matrix ( identity )

initialWorld :: World
initialWorld = World { worldTime = 0.0,
                       worldWindowWidth = fromIntegral $ configWindowWidth defaultConfig,
                       worldWindowHeight = fromIntegral $ configWindowHeight defaultConfig,
                       worldCamera = buildCamera eye g,
                       worldObjects = objects,
                       worldLight = light,
                       worldClickPos = Nothing,
                       worldDebugIntersections = Nothing }
    where
        eye = V4 2.0 4.0 2.0 1.0
        g = V4 0.0 0.0 2.0 1.0
        colour1 = colour 255 25 80 255
        colour2 = colour 0 0 255 255
        sphere1Props = Properties { propDensity = 1.0,
                                    propReflectivity = 0.0,
                                    propSpecCoeff = 0.02,
                                    propDiffCoeff = 0.68,
                                    propAmbCoeff = 0.3, 
                                    propSpecColour = colour1,
                                    propDiffColour = colour1,
                                    propAmbColour = colour1,
                                    propF = 10.0 }
        planeProps = Properties { propDensity = 1.0,
                                  propReflectivity = 0.0,
                                  propSpecCoeff = 0.2,
                                  propDiffCoeff = 0.4,
                                  propAmbCoeff = 0.4, 
                                  propSpecColour = colour2,
                                  propDiffColour = colour2,
                                  propAmbColour = colour2,
                                  propF = 10.0 }
        objects = [sphere sphere1Props (translate 0.0 0.0 2.0),
                   sphere sphere1Props (translate (-3.0) 0.01 2.0),
                   infPlane planeProps identity]
        light = Light (V4 2.0 2.0 2.0 1.0) (colour 100 100 100 255) (V4 0.1 0.1 0.1 255)

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
        = let l = Light (V4 lx (ly + 0.1) lz lw) c i in
            world { worldLight = l }

    | G.EventKey (G.Char 'h') G.Down _ _        <- event
        = let l = Light (V4 (lx - 0.1) ly lz lw) c i in
            world { worldLight = l }

    | G.EventKey (G.Char 'j') G.Down _ _        <- event
        = let l = Light (V4 lx (ly - 0.1) lz lw) c i in
            world { worldLight = l }

    | G.EventKey (G.Char 'k') G.Down _ _        <- event
        = let l = Light (V4 (lx + 0.1) ly lz lw) c i in
            world { worldLight = l }

    | G.EventKey (G.Char 'o') G.Down _ _        <- event
        = let l = Light (V4 lx ly (lz + 0.1) lw) c i in
            world { worldLight = l }

    | G.EventKey (G.Char 'l') G.Down _ _        <- event
        = let l = Light (V4 lx ly (lz - 0.1) lw) c i in
            world { worldLight = l }


    | G.EventKey (G.MouseButton G.LeftButton)
                     G.Down _ (x, y) <- event
        = let
            world' = world { worldClickPos = Just (x, y) }
            wWidth = worldWindowWidth world
            wHeight = worldWindowHeight world
            actualPos = (x + wWidth / 2.0, y + wHeight / 2.0)
            ray = getFirstRay world actualPos
            intersections = debugRayPoints 0 world ray
            world'' =  world' { worldDebugIntersections = Just $ intersections }
          in
              unsafePerformIO $ do
                  print actualPos
                  return $
                    world''

    | otherwise = world
    where
        camera@(Camera eye@(V4 ex ey ez ew) g@(V4 gx gy gz gw) _ _ _ _ _ _ _ _ _ _) = worldCamera world
        light@(Light (V4 lx ly lz lw) c i) = worldLight world

advanceWorld :: Float -> World -> World
advanceWorld time world = world
        
        

main :: IO ()
main = G.playField
        (G.InWindow "Raytracer" (configWindowWidth defaultConfig, configWindowHeight defaultConfig) (10, 10))
        zoom
        (configStepsPerSecond defaultConfig)
        initialWorld
        debugAndTrace
        handleEvent
        advanceWorld
    where
        zoom = (configZoom defaultConfig, configZoom defaultConfig)