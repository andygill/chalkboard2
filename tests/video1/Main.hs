module Main where

import Graphics.ChalkBoard
import Graphics.ChalkBoard.Video
import Graphics.ChalkBoard.Shader

import Control.Concurrent
import Control.Monad ( when )
import System.Directory ( doesFileExist )
import System.Exit ( exitWith, ExitCode(..) )
import Data.IORef

--import Graphics.Rendering.OpenGL as GL hiding (uniform)



main = startChalkBoard [BoardSize (480) (360)] videoMain




videoMain cb = do
    exists <- doesFileExist $ "bigfoot.mpeg"
    when (not exists) $ do
            print "Error: bigfoot.mpeg is missing."
            exitWith (ExitFailure 1)
    
    
    
    -- Test Video Input
    videoPipe <- openVideoInPipe (ffmpegInCmd "bigfoot.mpeg")
    
    (Just buffer0) <- nextPPMFrame videoPipe
    (Just buffer1) <- nextPPMFrame videoPipe
    (Just buffer2) <- nextPPMFrame videoPipe
    
    drawChalkBuffer cb buffer0
    writeChalkBoard cb "inTest1.png"
    drawChalkBuffer cb buffer1
    writeChalkBoard cb "inTest2.png"
    drawChalkBuffer cb buffer2
    writeChalkBoard cb "inTest3.png"
    
    closeVideoInPipe videoPipe
    
    
    
    -- Test Video Output
    (w,h,img) <- readNormalizedBoard ("lambda.png")
    startDefaultWriteStream cb "outTest1.avi"
    
    let example x = unAlpha <$> (rotate (4*x) (scale (abs x) img)) `over` (boardOf (alpha (o (RGB x x (abs x)))))
    sequence_ [ drawChalkBoard cb (example (sin x)) | x <- [0,0.01..2] ]
    
    endWriteStream cb
    
    
    
    --Test Video End to End
    videoPipe2 <- openVideoInPipe "ffmpeg -i bigfoot.mpeg -f image2pipe -vcodec ppm -"
    startMyWriteStream cb "ffmpeg -f image2pipe -vcodec ppm -i - -f h264 -mbd rd -flags +4mv+aic -trellis 2 -cmp 2 -subcmp 2 -g 300 -pass 1/2 endToEnd.avi"
    
    let endToEndTest = do
                maybeBuffer <- nextPPMFrame videoPipe2
                case maybeBuffer of
                        (Nothing)     -> return ()
                        (Just buffer) -> do 
                                        drawChalkBuffer cb buffer
                                        endToEndTest
    
    endToEndTest
    endWriteStream cb
    closeVideoInPipe videoPipe2
    
    
    
    -- Multiple Input Streams
    videoPipe3 <- openVideoInPipe (ffmpegInCmd "bigfoot.mpeg")
    videoPipe4 <- openVideoInPipe "ffmpeg -i bigfoot.mpeg -f image2pipe -vcodec ppm -"
    
    mix <- mkMix
    (Just bufferS) <- nextPPMFrame videoPipe3
    
    startDefaultWriteStream cb "multInTest.avi"

    let multInputTest bufferP = do
                maybeBuffer1 <- nextPPMFrame videoPipe3
                case maybeBuffer1 of
                        (Nothing)     -> return ()
                        (Just buffer1) -> do
                                maybeBuffer2 <- nextPPMFrame videoPipe4
                                case maybeBuffer2 of
                                        (Nothing)     -> return ()
                                        (Just buffer2) -> do 
                                                drawChalkBuffer cb (boardToBuffer (0,0) (480,360) $ move (0,360) $ 
                                                        (mix (scaleXY (1,-1) $ bufferOnBoard buffer1 (boardOf white))
                                                             (scaleXY (1,-1) $ bufferOnBoard bufferS (boardOf white))
                                                             (scaleXY (1,-1) $ bufferOnBoard bufferP (boardOf white))
                                                             (scaleXY (1,-1) $ bufferOnBoard buffer2 (boardOf white))
                                                        ))
    
    multInputTest bufferS
    
    endWriteStream cb
    closeVideoInPipe videoPipe3
    closeVideoInPipe videoPipe4
    
    
    exitChalkBoard cb
    
    
    
    
    
mkMix :: IO (Board RGB -> Board RGB -> Board RGB -> Board RGB -> Board RGB)
mkMix = do 
	fileContents <- readFile "laplacian2.fs"
	return $ \ b1 b2 b3 b4 -> gslBoard fileContents
				[ ("sampler0",board b1)
				, ("sampler1",board b2)
				, ("sampler2",board b3)
				, ("sampler3",board b4)
				]
				[ ("tc_offset",uniform $ 
						ArrVec2 [ (x*(1/480),y*(1/360))
						        | y <- [-1,0,1],x <- [-1,0,1]
						        ] 
				  ) ]




    {-
    morph <- mkMix
    let merge = \ b1 -> gslBoard msg [ ("tex0",board b1) ] []

    (Just buffer0) <- nextPPMFrame videoPipe

    startDefaultWriteStream cb "test.mp4"

    let loopEnd = do
                endWriteStream cb
                exitChalkBoard cb
                                                
        loop1 bufferP = do
                maybeBuffer <- nextPPMFrame videoPipe
                case maybeBuffer of
                        (Nothing)     -> finish2 bufferP
                        (Just buffer) -> do 
                                        drawChalkBuffer cb (boardToBuffer (0,0) (480,360) $ move (0,360) $ 
                                                (morph 	(scaleXY (1,-1) $ bufferOnBoard buffer (boardOf white))
                                                        (scaleXY (1,-1) $ bufferOnBoard buffer0 (boardOf white))
                                                        (scaleXY (1,-1) $ bufferOnBoard bufferP (boardOf white))
                                                ))
                                        loop2 buffer
        finish1 bufferP = do
                maybeBuffer <- nextPPMFrame videoPipe
                case maybeBuffer of
                        (Nothing)     -> loopEnd
                        (Just buffer) -> do 
                                        drawChalkBuffer cb (boardToBuffer (0,0) (480,360) $ move (0,360) $ 
                                                (morph 	(scaleXY (1,-1) $ bufferOnBoard buffer (boardOf white))
                                                        (scaleXY (1,-1) $ bufferOnBoard buffer0 (boardOf white))
                                                        (scaleXY (1,-1) $ bufferOnBoard bufferP (boardOf white))
                                                ))
                                        finish1 buffer
        
        loop2 bufferP = do
                curCount <- readIORef count
                writeIORef count (curCount+0.01)
                let x = sin curCount
                maybeBuffer <- nextPPMFrame videoPipe2
                print "test"
                case maybeBuffer of
                        (Nothing)     -> finish1 bufferP
                        (Just buffer) -> do drawChalkBuffer cb (boardToBuffer (0,0) (480,360) $ move (0,360) $
                                                (merge (scaleXY (1,-1) $ bufferOnBoard buffer (boardOf white)))
                                                )
                                         {-
                                         do drawChalkBuffer cb (boardToBuffer (0,0) (480,360) $ move (0,360) $
                                                unAlpha <$> ((rotate (4*x) (scale (abs x) img)) `over`
                                                    (merge (scaleXY (1,-1) $ bufferOnBoard buffer (boardOf white)))
                                                ))
                                          --}
                                            loop1 buffer
        finish2 bufferP = do
                maybeBuffer <- nextPPMFrame videoPipe2
                case maybeBuffer of
                        (Nothing)     -> loopEnd
                        (Just buffer) -> do drawChalkBuffer cb (boardToBuffer (0,0) (480,360) $ move (0,360) $
                                                (merge (scaleXY (1,-1) $ bufferOnBoard buffer (boardOf white)))
                                                )
                                            finish2 buffer
        
        
    loop1 buffer0





msg = "uniform sampler2D tex0;\n" ++
        "void main(void) {\n" ++
        " gl_FragColor.rgb = 1.0 - texture2D(tex0,gl_TexCoord[0].st).rgb;\n" ++ -- 1.0 - gl_Color.rgb;\n" ++
--      " gl_FragColor.rgb = vec3(gl_TexCoord[0].s,,0.1);\n" ++ -- 1.0 - gl_Color.rgb;\n" ++
        " gl_FragColor.a = 1.0;\n" ++
        "}\n"




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

-}

