module Config where

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
                         configStepsPerSecond = 2, 
                         configBounces = 3, 
                         configZoom = 1 }
    where
        near = 1.0
        theta = 45.0
        aspect = 1.5
        height = near * tan(pi / 180.0 * theta / 2.0)
        width = height * aspect