module Main where

import Graphics.ChalkBoard
import Graphics.ChalkBoard.Video


main = startChalkBoard [] videoMain


videoMain cb = do
--    videoPipe <- openVideoInPipe "ffmpeg -i bigfoot.mpeg -f image2pipe -vcodec ppm  -"
    videoPipe <- openVideoInPipe "cat bigfoot.ppm"
    let loop 500 = exitChalkBoard cb
	loop n = do
    	(worked, buffer) <- nextPPMFrame videoPipe
    	drawChalkBuffer cb buffer
	loop (n+1)
	
    loop 0

    exitChalkBoard cb
