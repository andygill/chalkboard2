module Main where

import Graphics.ChalkBoard
import Control.Applicative

example1 :: Point -> Point -> R -> Active (Board Bool)
example1 s e th = id
 	$ fmap (\ (a,b) -> straightLine (s,(a,b)) th) 
	$ scale 1
	$ actLerp age s e

example2 :: O RGB -> Active (Board RGB)
example2 rgb 
	 = fmap (scale 0.2)
	 $ fmap (choose rgb white .$)
	 $ flicker [ example1 f' t' 0.05
		   | (f,t,i) <- zip3 srcs targets [0..15]
		   , let (f',t') = scale ((60-fromInteger i)/60) (f,t)		       
		   ]
 where
	srcs = [ (-1,1), (1,1), (1,-1), (-1,-1) ] ++ srcs
	targets = tail srcs
	
example3 :: R -> O RGB -> Active (Board RGB)
example3 r rgb = rot <*> ex
  where
	ex = example2 rgb
	rot = fmap (\ ui -> rotate (pi * ui * r)) age `streach` ex



examples = turnPages lerpAct [example3 0 red,example3 1 yellow,example3 0 blue]


main = startChalkBoard [] $ \ cb -> do
	let loop (brd:rest) = do
		drawChalkBoard cb brd
		loop rest
	    loop [] = return ()

--	startMyWriteStream cb "ffmpeg -f image2pipe -vcodec ppm -i - -vcodec libx264 -b 500k -vpre hq -vpre main square.mp4" 
	loop (simulate 10 examples)
--	endWriteStream cb

	exitChalkBoard cb
