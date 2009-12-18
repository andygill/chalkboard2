module Main where

import Graphics.ChalkBoard
import Graphics.ChalkBoard.Video
import Control.Concurrent

main = startChalkBoard [BoardSize (480*2) (360*2)] videoMain



videoMain cb = do
--    videoPipe <- openVideoInPipe "ffmpeg -i bigfoot.mpeg -f image2pipe -vcodec ppm  -"
    videoPipe <- openVideoInPipe "cat bigfoot.ppm"
    msg' <- readFile "laplacian.fs"
    let morph = hook2 msg'

    (worked, buffer0) <- nextPPMFrame videoPipe

    let loop 500 = exitChalkBoard cb
	loop n = do
    	  (worked, buffer) <- nextPPMFrame videoPipe
    	  drawChalkBuffer cb (boardToBuffer (0,0) (480*2,360*2) $ move (0,360*2) $ 
					(morph <$>) $
					(ozip
						(scaleXY (2,-2) $ bufferOnBoard buffer (boardOf white))
						(scaleXY (2,-2) $ bufferOnBoard buffer0 (boardOf white))))
{-
    	  drawChalkBuffer cb (boardToBuffer (0,0) (480*2,360*2) 
					$ move (0,360*2) 
					$ (morph <$>) 
					(scaleXY (2,-2) $ bufferOnBoard buffer (boardOf white)))
-}
--  	  threadDelay (1000 * 1000)
--	  _ <- getLine
          loop (n+1)
	
    loop 0

--    exitChalkBoard cb

msg = "uniform sampler2D tex0;\n" ++

	"void main(void) {\n" ++
	" gl_FragColor.rgb = 1.0 - texture2D(tex0,gl_TexCoord[0].st).rgb;\n" ++ -- 1.0 - gl_Color.rgb;\n" ++
--	" gl_FragColor.rgb = vec3(gl_TexCoord[0].s,,0.1);\n" ++ -- 1.0 - gl_Color.rgb;\n" ++
	" gl_FragColor.a = 1.0;\n" ++
	"}\n"
	