module Main where

import Graphics.ChalkBoard as CB
import Control.Applicative (pure)


main = do
        font <- initFont "../../Arial.ttf" 0
        let (w,h) = (400,400)
        startChalkBoard [BoardSize w h] (\cb ->  animMain cb font 0.01 (w,h))


animMain cb font sz (w,h) = do
        --Set up the font labals
        (aLabel,aSP) <- label font sz ("a")
        (bLabel,bSP) <- label font sz ("b")
        (cLabel,cSP) <- label font sz ("c")
        (areaLabel,areaSP) <- label font sz ("area = c²")
        (formulaLabel,formulaSP) <- label font sz ("c² = a² + b²")
        
        --Set up the different parts of the animation
        let background = pure $ boardOf $ withAlpha 0.2 yellow :: Active (Board (RGBA->RGBA))
            
            triangle345 = triangle (-0.2,0.15) (-0.2,-0.15) (0.2,-0.15)
            triLines = pointsToLine [(-0.2,0.15), (-0.2,-0.15), (0.2,-0.15), (-0.2,0.15)] 0.004
            mainTriangle = (choose (alpha black) transparent <$> triLines) `over` (choose (alpha yellow) transparent <$> triangle345)
            largeTriangle = activeMove (0.15,0.2) $ activeScale (2/3) $ pure $ scale 1.5 $ mainTriangle
            
            a = move (-0.25,-0.03) $ makelbl aSP aLabel
            b = move (-0.04,-0.195) $ makelbl bSP bLabel
            c = move (0.005,0.005) $ makelbl cSP cLabel
            firstABC = activeTempAppear $ pure $ (move (-0.1,0) a) `over` (move (-0.03,-0.08) b) `over` (move (0.005,0.005) c)
            
            otherTriangles = [ rotate (-i*pi/2) $ move (0.15,0.2) $ mainTriangle | i <- [1..3] ]
            otherTrianglesActive = overList [actMove i $ activeAppear $ pure $ {-activeMove (1,0) $ move (-1,0) $-} tri | (tri,i) <- Prelude.zip otherTriangles [1..] ]
            
            colorSquare = activeAppear $ pure $ scale 0.095 $ choose (alpha yellow) transparent <$> square
            area = activeAppear $ pure $ move (-0.4,0.35) $ makelbl aSP areaLabel
            secondABC = activeTempAppear $ pure $ move (0.15, 0.2) $ (move (0.06,0) a) `over` (move (0,0.06) b) `over` c `over` (move (0.05,-0.38) c)
            
            fadedTriangles = [ rotate (-i*pi/2) $ move (0.15,0.2) $ choose (withAlpha 0.6 white) transparent <$> triangle345 | i <- [0,1] ]
            slideLeft = activeAppear $ (activeMove (-0.3,-0.4) $ pure $ move (0.15,0.2) $ mainTriangle) `over` (pure (head fadedTriangles))
            slideRight = activeAppear $ (activeMove (0.4,-0.3) $ pure $ (head otherTriangles)) `over` (pure (last fadedTriangles))
            
            thirdABC = activeTempAppear $ pure $ (move (-0.15, -0.2) $ a `over` b) `over` (move (0.45,-0.36) a) `over` (move (0.4,0) b)
            
            square4 = move (0.15, -0.15) $ scale 0.4 $ square
            square3 = move (-0.2, -0.2) $ scale 0.3 $ square
            squares = (square4 `over` square3)
            newLines = pointsToLine [(-0.05,-0.35), (0.35,-0.35), (0.35,0.05), (-0.05,0.05), (-0.05,-0.35), (-0.35,-0.35), (-0.35,-0.05), (-0.05,-0.05)] 0.004
            newSquares = (fadeIn 1 black newLines) `over` (fadeIn 0.8 yellow squares)
            
            finalABC = activeAppear $ pure $ (move (-0.15, -0.2) a) `over` (move (0.03,-0.36) a) `over` (move (0.18,-0.2) b) `over` (move (0.4,0) b)
            formula = activeAppear $ pure $ move (0.15,0.35) $ makelbl aSP formulaLabel
        
        --Set up the animation ordering/timing
        let anim = flicker [ taking 0.5 $ background
                           , taking 1 $ firstABC
                           , taking 1 $ largeTriangle
                           , wait 0.5
                           , taking 2 $ otherTrianglesActive
                           , taking 1.5 $ colorSquare `over` secondABC `over` area
                           , taking 1 $ slideLeft
                           , wait 0.5
                           , taking 1 $ slideRight
                           , taking 1 $ thirdABC
                           , taking 1 $ newSquares `over` thirdABC
                           , taking 3 $ finalABC `over` formula
                           ]
        
        
        --Pick the animation you would like to see and turn it into a play object
	playObj <- byFrame 29.97 anim
        
        --Start the video write stream
        sid <- startDefaultWriteStream cb "pythagorean.avi"
        
        --Run the animation
        let loop = do
                mbScene <- play playObj
                case mbScene of
                        Just scene -> do
                                let scaledScene = scene
                                drawChalkBoard cb $ unAlphaBoard (boardOf white) scaledScene
                                frameChalkBoard cb sid
                                loop
                        Nothing  -> return ()
        
        loop
        
        endWriteStream cb sid
        exitChalkBoard cb




overList (b:[]) = b
overList (b:bs) = b `over` (overList bs)


fadeIn :: UI -> O RGB -> Board Bool -> Active (Board (RGBA -> RGBA))
fadeIn maxAlpha rgb brd = fmap (\ ui -> choose (withAlpha (o (ui*maxAlpha)) rgb) transparent <$> brd) age


wait :: R -> Active (Board (RGBA -> RGBA))
wait n = taking n (pure (boardOf transparent))



makelbl :: Float -> Board UI -> Board (RGBA -> RGBA)
makelbl size lbl =  color black $ scale (0.7) $ scale (1/(size*25)) $ lbl

color :: O RGB -> Board UI -> Board (RGBA -> RGBA)
color rgb brd = (\ ui -> withAlpha ui rgb) <$> brd


