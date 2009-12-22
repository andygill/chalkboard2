module Main where

import Graphics.ChalkBoard
import Graphics.ChalkBoard.Video
import Graphics.ChalkBoard.Shader
import Control.Concurrent
import Control.Monad ( when )
import Graphics.Rendering.OpenGL as GL hiding (uniform)

main = startChalkBoard [BoardSize (480) (360)] videoMain

mkMix :: IO (Board RGB -> Board RGB -> Board RGB -> Board RGB)
mkMix = do 
	fileContents <- readFile "laplacian.fs"
	return $ \ b1 b2 b3 -> gslBoard fileContents
				[ ("sampler0",board b1)
				, ("sampler1",board b2)
				, ("sampler2",board b3)
				]
				[ ("tc_offset",uniform $ 
						ArrVec2 [ (x*(1/480),y*(1/360))
						        | y <- [-1,0,1],x <- [-1,0,1]
						        ] 
				  ) ]

videoMain cb = do
--    videoPipe <- openVideoInPipe "ffmpeg -i bigfoot.mpeg -f image2pipe -vcodec ppm  -"
--    videoPipe <- openVideoInPipe "cat bigfoot.ppm"
    videoPipe <- openVideoInPipe (ffmpegInCmd "bigfoot.mpeg")
    
    morph <- mkMix

    (worked, buffer0) <- nextPPMFrame videoPipe

    startDefaultWriteStream cb "test.avi"

    let loop False _ = do
                endWriteStream cb
                exitChalkBoard cb
        loop True bufferP = do
                (worked, buffer) <- nextPPMFrame videoPipe
                drawChalkBuffer cb (boardToBuffer (0,0) (480,360) $ move (0,360) $ 
					(morph 	(scaleXY (1,-1) $ bufferOnBoard buffer (boardOf white))
						(scaleXY (1,-1) $ bufferOnBoard buffer0 (boardOf white))
						(scaleXY (1,-1) $ bufferOnBoard bufferP (boardOf white))
						))					
{-
                drawChalkBuffer cb (boardToBuffer (0,0) (480*2,360*2) 
					$ move (0,360*2) 
					$ (morph <$>) 
					(scaleXY (2,-2) $ bufferOnBoard buffer (boardOf white)))
-}
--              threadDelay (1000 * 1000)
--              _ <- getLine
                loop worked buffer
	
    loop worked buffer0

--    exitChalkBoard cb



msg = "uniform sampler2D tex0;\n" ++

	"void main(void) {\n" ++
	" gl_FragColor.rgb = 1.0 - texture2D(tex0,gl_TexCoord[0].st).rgb;\n" ++ -- 1.0 - gl_Color.rgb;\n" ++
--	" gl_FragColor.rgb = vec3(gl_TexCoord[0].s,,0.1);\n" ++ -- 1.0 - gl_Color.rgb;\n" ++
	" gl_FragColor.a = 1.0;\n" ++
	"}\n"
	
