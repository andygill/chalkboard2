module Main where

import Graphics.ChalkBoard
--import Control.Applicative


main = startChalkBoard [BoardSize 400 400] $ \ cb -> do
	let loop (brd:rest) = do
		drawChalkBoard cb brd -- $ unAlphaBoard (boardOf white) brd
		loop rest
	    loop [] = return ()

--	startMyWriteStream cb "ffmpeg -f image2pipe -vcodec ppm -i - -vcodec libx264 -b 500k -vpre hq -vpre main square.mp4" 
	loop (simulate 10 orbit)
--	endWriteStream cb

	--exitChalkBoard cb



orbit = flicker [
                        --(fmap (mix white green .$) $ (fmap (\ ui -> scale (0.1) $ (choose (o ui) (o 0) <$> circle)) $ scale 8 $ fadein 1)),
                        --(fmap (mix white blue .$) $ (fmap (\ ui -> move (0,0.2) $ scale (0.02) $ (choose (o ui) (o 0) <$> circle)) $ scale 8 $ fadein 1))
                        (fmap (mix white blue .$) 
                        $ (fmap (\ x -> move ((cos (x*2*pi))/6,(sin x*2*pi)/6) $ scale (0.02) $ (choose (o 1) (o 0) <$> circle)) 
                        (scale 20 $ fadein 1)))
                ]
        

{-      
example1 :: Point -> Point -> R -> Active (Board Bool)
example1 s e th = id
 	$ fmap (\ (a,b) -> straightLine (s,(a,b)) th) 
	$ scale 1
	$ actLerp balloon s e
-}
