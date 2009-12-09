module Main where

import Graphics.ChalkBoard
import Graphics.ChalkBoard.Video


main = startChalkBoard [] videoMain


videoMain cb = do
    videoPipe <- openVideoInPipe "cat ex5.ppm"
    (worked, buffer) <- nextPPMFrame videoPipe
    
    drawChalkBuffer cb buffer
    writeChalkBoard cb "test.png"
    
    exitChalkBoard cb
