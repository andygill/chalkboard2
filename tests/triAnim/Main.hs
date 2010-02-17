module Main where

import Graphics.ChalkBoard as CB
import Control.Applicative (pure)
import System.IO.Unsafe


main = startChalkBoard [BoardSize (400) (400)] animMain


animMain cb = do
        let background = pure $ boardOf (alpha white)
            triangle345 = pure $ choose (alpha black) transparent <$> triangle (0,0) (0,-0.3) (0.4,-0.3)
            square4  = move (0.2,-0.5) $ scale 0.4 $ choose (withAlpha 0.6 blue) transparent <$> square
            
        let --square3  = mix white red <$> {-myMix <$> (CB.zip (CB.zip (boardOf (withAlpha 0.6 red)) (boardOf transparent))-} (withDefault 0.0 <$> (overList init3pos))
            square3 :: Active (Board (RGBA->RGBA)) -- Fix this stuff
            square3  = choose (withAlpha 0.6 red) transparent <$> (overList moveSq3s)
            moveSq3s = [ (moveSq3 curSq i) | (curSq,i) <- Prelude.zip square3s [0..] ]
            square3s = [ scale 0.3 $ scale (1/3) $ square | (x,y) <- combinations [0..2] [0..2] ]
            
            --moveSq3s = [ unsafePerformIO (moveSquare (moveSq3 curSq i)) | (curSq,i) <- Prelude.zip square3s [0..] ]
            --init3pos = map (\x -> move (-0.15,-0.15) $ scale 0.3 x) square3s
            --square3s = [ move (x/3,y/3) $ move (-1/3,-1/3) $ scale (1/3) $ {-withMask 0.6 <$>-} square | (x,y) <- combinations [0..2] [0..2] ]
            
        
        let start = square4 `over` square3 `over` triangle345 `over` background
        
        drawChalkBoard cb $ unAlphaBoard (boardOf white) (scale 0.8 $ move (-0.2,0.15) $ start)



overList (b:[]) = b
overList (b:bs) = b `over` (overList bs)

combinations (a:as) (b:bs) = (a,b):((combinations as (b:bs)) ++ (combinationsR (a:as) bs))
combinations _ _ = []
combinationsR (a:as) (b:bs) = (a,b):(combinationsR (a:as) bs)
combinationsR _ _ = []   



moveSquare active = do
                frame <- byFrame 29.97 active
                --frame <- realTime active
                mbBrd <- play frame
                finalBrd <- case mbBrd of
                                Just brd -> return brd
                                Nothing  -> return $ boardOf (o False)
                return finalBrd


moveSq3 :: Board a -> Int -> Active (Board a)
moveSq3 sq i = moveBrd sq (pi/2-atan(4/3)) (x*0.1-0.25,y*0.1-0.25) (0.15,0.15)
        where x = fromIntegral (i `mod` 3)
              y = fromIntegral (i `div` 3)

moveBrd :: Board a -> Radian -> Point -> Point -> Active (Board a)
moveBrd brd rot (sx,sy) (ex,ey) = mkActive 1 brd activeBoth
        where activeMove ui = move (ui*(ex-sx)+sx,ui*(ey-sy)+sy)
              activeRot ui  = rotate (ui * rot)
              activeBoth ui = (\x -> (activeMove ui) ((activeRot ui) x))


mkActive :: R -> (Board a) -> (UI -> Board a -> Board a) -> Active (Board a)
mkActive t brd fn = ( fmap (\ x -> (fn x) $ brd)
	                (scale t $ age)
                    )






{-
        moveSq3 sq i = foldr1 page
               [
                
               ]

        where movedSq = rotate (pi/2-atan(4/3)){-0.6435-} $ move (0.4,0.4) $ sq
              x = i `mod` 3
              y = i `div` 3
              
              
              

let fn :: O ((RGB,RGB),Bool) -> O RGB
		    fn o = choose (fstO (fstO o)) (sndO (fstO o)) (sndO o)
		    drawChalkBoard cb $ fn <$> ((brd1 `CB.zip` brd2) `CB.zip` brd3)
		    
myMix :: O ((RGBA->RGBA,RGBA->RGBA),UI) -> O (RGBA->RGBA)
myMix x = choose (fstO (fstO x)) (sndO (fstO x)) (o ((sndO x) == 1))
--}


