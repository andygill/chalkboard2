module Main where

import Graphics.ChalkBoard as CB
import Control.Applicative (pure)


main = do
        font <- initFont "../../Arial.ttf" 0
        let (w,h) = (400,400)
        startChalkBoard [BoardSize w h] (\cb ->  animMain cb font 0.03 (w,h))


animMain cb font sz (w,h) = do
        (aLabel,aSP) <- label font sz ("a")
        (bLabel,bSP) <- label font sz ("b")
        (cLabel,cSP) <- label font sz ("c")
        (areaLabel,areaSP) <- label font sz ("area = c²")
        (formulaLabel,formulaSP) <- label font sz ("c² = a² + b²")
        
        
        let background = pure $ boardOf $ withAlpha 0.2 yellow :: Active (Board (RGBA->RGBA))
        
            triangle345 = triangle (-0.2,0.15) (-0.2,-0.15) (0.2,-0.15)
            triLines = pointsToLine [(-0.2,0.15), (-0.2,-0.15), (0.2,-0.15), (-0.2,0.15)] 0.004
            mainTriangle = (choose (alpha black) transparent <$> triLines) `over` (choose (alpha yellow) transparent <$> triangle345)
            
            largeTriangle = mergeActive (activeMove (0.15,0.2)) $ mkActive (activeScale (2/3)) $ scale 1.5 $ mainTriangle
            a = makelbl sz $ move (-0.25,0) aLabel
            b = makelbl sz $ move (0,-0.2) bLabel
            c = makelbl sz $ move (0.03,0.03) cLabel

            otherTriangles = [ rotate (-i*pi/2) $ move (0.15,0.2) $ mainTriangle | i <- [1..3] ]
            otherTrianglesIn = overList [actMove i $ mkActive appear $ {-mkActive (activeMove (1,0)) $ move (-1,0) $-} tri | (tri,i) <- Prelude.zip otherTriangles [1..] ]
            
            colorSquare = fadeIn 1 yellow $ scale 0.095 $ square
            
            fadedTriangles = [ rotate (-i*pi/2) $ move (0.15,0.2) $ choose (withAlpha 0.6 white) transparent <$> triangle345 | i <- [0,1] ]
            slideLeft = mergeActive appear $ (mkActive (activeMove (-0.3,-0.4)) $ move (0.15,0.2) $ mainTriangle) `over` (pure (head fadedTriangles))
            slideRight = mergeActive appear $ (mkActive (activeMove (0.4,-0.3)) $ (head otherTriangles)) `over` (pure (last fadedTriangles))
            
            square4 = move (0.15, -0.15) $ scale 0.4 $ square
            square3 = move (-0.2, -0.2) $ scale 0.3 $ square
            squares = (square4 `over` square3)
            
            fadeSquares = fadeIn 0.5 white squares
            
            newLines = pointsToLine [(-0.05,-0.35), (0.35,-0.35), (0.35,0.05), (-0.05,0.05), (-0.05,-0.35), (-0.35,-0.35), (-0.35,-0.05), (-0.05,-0.05)] 0.004
            newSquares = (fadeIn 1 black newLines) `over` (fadeIn 0.9 yellow squares)

        let anim = for 10 $ flicker [ taking 0.5 $ background
                                    , taking 0.5 $ a `over` b `over` c
                                    , taking 1 $ largeTriangle
                                    , wait 0.5
                                    , taking 1 $ otherTrianglesIn  -- Need to fade this in or something
                                    , taking 0.5 $ colorSquare
                                    , wait 0.5
                                    , taking 1 $ slideLeft
                                    , wait 0.5
                                    , taking 1 $ slideRight
                                    --, taking 0.5 $ fadeSquares
                                    , wait 0.5
                                    , taking 1 $ newSquares
                                    , wait 0.5
                                    ]
        
        
        --Pick the animation you would like to see and turn it into a play object
	playObj <- byFrame 29.97 anim
        
        
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
activeRotate radians ui = rotate (ui*radians)
activeScale s ui = scale (ui*s + (1-ui))

appear ui = if ui == 0
                then scale 0
                else scale 1


mergeActive :: (UI -> Board a -> Board a) -> Active (Board a) -> Active (Board a)
mergeActive fn (Active start stop f) = Active start stop (\ui -> (fn (fromRational ui) . f) ui)

mkActive :: (UI -> Board a -> Board b) -> (Board a) -> Active (Board b)
mkActive fn brd = fmap (\ ui -> (fn ui) $ brd) age


fadeIn :: UI -> O RGB -> Board Bool -> Active (Board (RGBA -> RGBA))
fadeIn maxAlpha rgb brd = fmap (\ ui -> choose (withAlpha (o (ui*maxAlpha)) rgb) transparent <$> brd) age


wait :: R -> Active (Board (RGBA -> RGBA))
wait n = taking n (pure (boardOf transparent))



makelbl :: Float -> Board UI -> Active (Board (RGBA -> RGBA))
makelbl size lbl =  pure $ color black $ scale (0.7) $ scale (1/size) $ lbl

color :: O RGB -> Board UI -> Board (RGBA -> RGBA)
color rgb brd = (\ ui -> withAlpha ui rgb) .$ brd


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




