module Main where

import Graphics.ChalkBoard as CB
import Control.Applicative (pure)
--import System.IO.Unsafe


main = startChalkBoard [BoardSize (400) (400)] animMain


animMain cb = do
        --Set up the animations
        let triangle345 = pure $ choose (alpha black) transparent <$> triangle (0,0) (0,-0.3) (0.4,-0.3) :: Active (Board (RGBA->RGBA))
            
            moveSq3s = [ taking 0.5 (moveSq3 curSq i) | (curSq,i) <- Prelude.zip square3s [0..] ]
            square3s = [ scale 0.3 $ scale (1/3) $ choose (withAlpha 0.6 red) transparent <$> square | (x,y) <- combinations [0..2] [0..2] ]
            
            moveSq4s = [ taking 0.3 (moveSq4 curSq i) | (curSq,i) <- Prelude.zip square4s [0..] ]
            square4s = [ scale 0.4 $ scale (1/4) $ choose (withAlpha 0.6 blue) transparent <$> square | (x,y) <- combinations [0..3] [0..3] ]
            
            moveRec4s = [ taking 1 (moveRec4 curSq i) | (curSq,i) <- Prelude.zip rectangle4s [0..] ]
            rectangle4s = [ scale 0.4 $ scaleXY (1/4,1) $ choose (withAlpha 0.6 blue) transparent <$> square | (x,y) <- combinations [0..1] [0..1] ]
            
            -- could use something instead of combinations or actually do something with them?
            -- could start square moves from their start locations and then move from 0,0 to final location?
            -- could potentially use UI instead, not that it would be helpful
        
	
	--Determine the ordering/timing to create different animations
	let normalAnim = for 6 $ (flicker moveSq4s) `over` (flicker moveSq3s) `over` triangle345
	    allAtOnce = for 6 $ (taking 1 $ overList moveSq4s) `over` (taking 1 $ overList moveSq3s) `over` triangle345
	    redThenBlue = for 8 $ flicker $ [taking 0 triangle345] ++ (map (taking 0.3) moveSq3s) ++ moveSq4s
	    blueThenRed = for 8 $ flicker $ [taking 0 triangle345] ++ moveSq4s ++ (map (taking 0.3) moveSq3s)
	    rectangles = for 6 $ (flicker moveRec4s) `over` (flicker moveSq3s) `over` triangle345
	    rectsAtOnce = for 6 $ (taking 1 $ overList moveRec4s) `over` (taking 1 $ overList moveSq3s) `over` triangle345
	    
	    combineSome = for 6 $ (flicker [(overList (take 2 (drop i moveSq4s))) | i <- [0,2..14] ]) `over` (flicker moveSq3s) `over` triangle345
	    -- would be interesting to maybe map some 'sleep's into some of the ones that are just using 'over' and going at the same time
	
	
	--Pick the animation you would like to see and turn it into a play object
	playObj <- byFrame 29.97 normalAnim
        --playObj <- realTime active
        
        
        --Run the animation
        let loop = do
                mbScene <- play playObj
                case mbScene of
                        Just scene -> do
                                let scaledScene = scale 0.8 $ move (-0.2,0.15) $ scene
                                drawChalkBoard cb $ unAlphaBoard (boardOf white) scaledScene
                                loop
                        Nothing  -> return ()
        
        loop
        
        exitChalkBoard cb



overList (b:[]) = b
overList (b:bs) = b `over` (overList bs)

combinations (a:as) (b:bs) = (a,b):((combinations as (b:bs)) ++ (combinationsR (a:as) bs))
combinations _ _ = []
combinationsR (a:as) (b:bs) = (a,b):(combinationsR (a:as) bs)
combinationsR _ _ = []



moveSq3 :: Board a -> Int -> Active (Board a)
moveSq3 sq i = moveBrd sq (pi/2-atan(4/3)) (x*0.1-0.25,y*0.1-0.25) (x*0.1+0.15,y*0.1+0.15)
        where x = fromIntegral (i `mod` 3)
              y = fromIntegral (i `div` 3)

moveSq4 :: Board a -> Int -> Active (Board a)
moveSq4 sq i = moveBrd sq (pi/2-atan(4/3)) (x*0.1+0.05,-y*0.1-0.35) ((fx i)*0.1+0.05,(fy i)*0.1+0.05)
        where x = fromIntegral (i `mod` 4)
              y = fromIntegral (i `div` 4)
              fx index | index < 5  = fromIntegral index
                       | index > 10 = fromIntegral (index-11)
                       | odd index  = 0
                       | even index = 4
              fy index | index < 5   = 0
                       | index < 7   = 1
                       | index < 9   = 2
                       | index < 11  = 3
                       | index >= 11 = 4

moveRec4 :: Board a -> Int -> Active (Board a)
moveRec4 rec i = case num of
        0 -> moveBrd rec angle        (num*0.1+0.05,0.1-0.6) (0.05,0.2)
        1 -> moveBrd rec (angle+pi/2) (num*0.1+0.05,0.1-0.6) (-0.05,0.2+0.1)
        2 -> moveBrd rec angle        (num*0.1+0.05,0.1-0.6) (0.05+0.4,0.2+0.1)
        3 -> moveBrd rec (angle+pi/2) (num*0.1+0.05,0.1-0.6) (-0.05-0.4,0.2)
        where angle = (pi/2-atan(4/3))
              num = fromIntegral i



moveBrd :: Board a -> Radian -> Point -> Point -> Active (Board a)
moveBrd brd rot (sx,sy) (ex,ey) = mkActive brd activeBoth
        where activeMove ui = move (ui*(ex-sx)+sx,ui*(ey-sy)+sy)
              activeRot ui  = rotate (ui * rot)
              activeBoth ui board = (activeRot ui) $ (activeMove ui) board



mkActive :: (Board a) -> (UI -> Board a -> Board a) -> Active (Board a)
mkActive brd fn = fmap (\ ui -> (fn ui) $ brd) age





{-
mkActive, color, overList, unfilledPolygon?
moveToOrigin? or a localRotate? or a moveToPosition?
compiler optimizations would be great since we are often manipulating the same board over and over



color :: O RGB -> Active (Board UI) -> Active (Board (RGBA -> RGBA))
color rgb = fmap ((\ ui -> withAlpha ui rgb) .$)

let fn :: O ((RGB,RGB),Bool) -> O RGB
fn o = choose (fstO (fstO o)) (sndO (fstO o)) (sndO o)
drawChalkBoard cb $ fn <$> ((brd1 `CB.zip` brd2) `CB.zip` brd3)

myMix :: O ((RGBA->RGBA,RGBA->RGBA),UI) -> O (RGBA->RGBA)
myMix x = choose (fstO (fstO x)) (sndO (fstO x)) (o ((sndO x) == 1))
--}





