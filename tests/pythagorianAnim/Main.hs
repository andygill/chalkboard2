module Main where

import Graphics.ChalkBoard as CB
import Control.Applicative (pure)


main = startChalkBoard [BoardSize (400) (400)] animMain


animMain cb = do
        let background = pure $ boardOf $ withAlpha 0.2 yellow :: Active (Board (RGBA->RGBA))
            triangle345 = choose (alpha yellow) transparent <$> triangle (-0.2,0.15) (-0.2,-0.15) (0.2,-0.15)
            triLines = choose (alpha black) transparent <$> pointsToLine [(-0.2,0.15), (-0.2,-0.15), (0.2,-0.15), (-0.2,0.15)] 0.004
            mainTriangle = triLines `over` triangle345
            largeTriangle = mergeActive (activeMove (0.15,0.2)) $ mkActive (activeScale (2/3)) $ scale 1.5 $ mainTriangle

            otherTriangles = mkActive (activeScale 2) $ overList [rotate (i*pi/2) $ move (0.15,0.2) $ mainTriangle | i <- [1..3] ]

        let anim = for 4 $ flicker [ actMove 1 $ taking 1 $ largeTriangle
                                   , taking 1 $ otherTriangles] -- Need to fade this in or something
        
        
        --Pick the animation you would like to see and turn it into a play object
	playObj <- byFrame 29.97 (anim `over` background)
        
        
        --Run the animation
        let loop = do
                mbScene <- play playObj
                case mbScene of
                        Just scene -> do
                                let scaledScene = scene
                                drawChalkBoard cb $ unAlphaBoard (boardOf white) scaledScene
                                loop
                        Nothing  -> return ()
        
        loop
        
        exitChalkBoard cb


overList (b:[]) = b
overList (b:bs) = b `over` (overList bs)


activeMove (x,y) ui = move (ui*x,ui*y)
activeRotate radians ui = rotate (ui * radians)
activeScale s ui = scale (ui*s + (1-ui))


mergeActive :: (UI -> Board a -> Board a) -> Active (Board a) -> Active (Board a)
mergeActive fn (Active start stop f) = Active start stop (\ui -> (fn (fromRational ui) . f) ui)

mkActive :: (UI -> Board a -> Board a) -> (Board a) -> Active (Board a)
mkActive fn brd = fmap (\ ui -> (fn ui) $ brd) age


{-
newActiveMove :: Point -> Board a -> Active (Board a)
activeMove (x,y) brd = mkActive brd activeMv
        where activeMv ui = move (ui*x,ui*y)

newActiveRotate :: Radian -> Board a -> Active (Board a)
activeRotate radians brd = mkActive brd activeRot
        where activeRot ui = rotate (ui * radians)

newActiveScale :: Float -> Board a -> Active (Board a)
activeScale s brd = mkActive brd activeScl
        where activeScl ui = scale (ui*s + (1-ui))
-}




