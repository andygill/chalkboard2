module Main where

import Graphics.ChalkBoard
import Control.Applicative

example2 :: Active (Board RGB)
example2 = fmap (scale 0.2)
	 $ fmap (choose red white .$)
	 $ flicker [ example1 f' t' 0.05
		   | (f,t,i) <- zip3 srcs targets [0..50]
		   , let (f',t') = scale ((60-fromInteger i)/60) (f,t)		       
		   ]
 where
	srcs = [ (-1,1), (1,1), (1,-1), (-1,-1) ] ++ srcs
	targets = tail srcs
	
example3 :: Active (Board RGB)
example3 = rot <*> example2
  where
	rot = fmap (\ ui -> rotate (pi * ui)) balloon `streach` example2
	
example1 :: Point -> Point -> R -> Active (Board Bool)
example1 s e th = id
 	$ fmap (\ (a,b) -> straightLine (s,(a,b)) th) 
	$ scale 1
	$ actLerp balloon s e

main = startChalkBoard [] $ \ cb -> do
	let loop (brd:rest) = do
		drawChalkBoard cb brd
		loop rest
	    loop [] = return ()

--	startMyWriteStream cb "ffmpeg -f image2pipe -vcodec ppm -i - -vcodec libx264 -b 500k -vpre hq -vpre main square.mp4" 
	loop (simulate 10 example3)
--	endWriteStream cb
	exitChalkBoard cb
