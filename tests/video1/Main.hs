module Main where

import Graphics.ChalkBoard
import Graphics.ChalkBoard.Video
import Control.Concurrent

main = startChalkBoard [BoardSize (480*2) (360*2)] videoMain


videoMain cb = do
    videoPipe <- openVideoInPipe "ffmpeg -i bigfoot.mpeg -f image2pipe -vcodec ppm  -"
--    videoPipe <- openVideoInPipe "cat bigfoot.ppm"
    let loop 500 = exitChalkBoard cb
	loop n = do
    	(worked, buffer) <- nextPPMFrame videoPipe
    	drawChalkBuffer cb (boardToBuffer (0,0) (480*2,360*2) $ move (0,360*2) $ scaleXY (2,-2) $ bufferOnBoard buffer (boardOf white))
--	threadDelay (1000 * 1000)
	loop (n+1)
	
    loop 0

    exitChalkBoard cb
