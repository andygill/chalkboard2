module Main where

import Graphics.ChalkBoard
--import Control.Applicative


main = startChalkBoard [BoardSize 400 400] $ \ cb -> do
	let loop (brd:rest) = do
		drawChalkBoard cb brd -- $ unAlphaBoard (boardOf white) brd
		loop rest
	    loop [] = return ()

--	startMyWriteStream cb "ffmpeg -f image2pipe -vcodec ppm -i - -vcodec libx264 -b 500k -vpre hq -vpre main orbit.mp4" 
	loop $ concat $ fmap (simulate 25) orbit
--	endWriteStream cb

	--exitChalkBoard cb



orbit = {-flicker-} [
                        (fmap (mix white green .$) $ (fmap (\ ui -> scale (0.2) $ (choose (o ui) (o 0) <$> circle)) $ scale 5 $ fadein 1)),
                        
                        (fmap (mix white blue .$) 
                        $ (fmap (\ x -> move ((cos (x*2*pi))*0.2,(sin (x*2*pi))*0.2) $ scale (0.02) $ (choose (o (x*5)) 0 <$> circle)) 
                        (scale 5 $ fadein 1))
                        `over`
                        (fmap (\x -> (choose (o x) 0 <$> (functionLine (\pt -> ((cos (pt*2*pi))*0.2,(sin (pt*2*pi))*0.2)) 0.0025 100))) $ fadein 1)
                        ),
                        
                        (fmap (mix white red .$) 
                        $ (fmap (\ x -> move ((cos (x*2*pi))*0.3-0.1,(sin (x*2*pi))*0.2) $ scale (0.02) $ (choose (o (x*5)) 0 <$> circle)) 
                        (scale 2.5 $ fadein 2))
                        `over`
                        (fmap (\x -> (choose (o (x*2)) 0 <$> (functionLine (\pt -> ( (cos (pt*2*pi))*0.3-0.1, abs (sin (pt*2*pi))*0.2 )) 0.0025 100))) $ fadein 2)
                        ),
                        
                        (fmap (mix white green .$) 
                        $ (fmap (\ x -> move (-(cos (x*2*pi))*0.4,-(sin (x*2*pi))*0.4) $ scale (0.02) $ (choose (o (x*10)) 0 <$> circle)) 
                        (scale 10 $ fadein 1))
                        `over`
                        (fmap (\x -> (choose (o x) 0 <$> (functionLine (\pt -> ((cos (pt*2*pi))*0.4,(sin (pt*2*pi))*0.4)) 0.0025 100))) $ fadein 1)
                        )
                ]
        

{-      
example1 :: Point -> Point -> R -> Active (Board Bool)
example1 s e th = id
 	$ fmap (\ (a,b) -> straightLine (s,(a,b)) th) 
	$ scale 1
	$ actLerp balloon s e
-}

{-
-- | A line generated by sampling a function from @R@ to @Point@s,
-- with a specific width. There needs to be at least 2 sample points.

functionLine :: (R -> Point) -> R -> Int -> Board Bool
functionLine line width steps = pointsToLine samples width
-}