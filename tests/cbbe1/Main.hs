{-# LANGUAGE ForeignFunctionInterface #-}
-- ChalkBoard Back End Example 1
-- August 2009
-- Kevin Matlage


module Main where

import Graphics.ChalkBoard
import Graphics.ChalkBoard.CBIR
import Graphics.ChalkBoard.OpenGL.CBBE
import Control.Concurrent.MVar
import Control.Concurrent
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUChar)
import Graphics.ChalkBoard.Core	-- TODO: remove, and fix with O
import Graphics.ChalkBoard.Video ( ffmpegOutCmd )



main = do startChalkBoard [] main2


-- Will need to fork off a thread to pass more instructions to the mvar if we want animation
-- Do we want an mvar for the board as well so that we can switch which board we are displaying?
main2 :: ChalkBoard -> IO ()
main2 cb = do
    let ex = example2
    let   animation speed 100 _ = drawRawChalkBoard cb [Exit]
	  animation speed count [] =  drawRawChalkBoard cb [Exit]
	  animation speed count (inst:insts) = do
               drawRawChalkBoard cb inst
               threadDelay speed
	       animation speed (succ count) insts
    animation 0{-1000000-} 2 (fst ex)


-- Hardcode example of some CBIR instructions
-- The first instruction is *always* setting the the viewing board (??)
example1:: ([[Inst BufferId]], BufferId)
example1 = ( 
             [[ Allocate 0 (sz,sz) RGBADepth (BackgroundRGBADepth (RGBA 1 1 1 1))
             , Allocate 1 (sz,sz) RGBADepth (BackgroundRGBADepth (RGBA 0 0 1 0.6))
             , Allocate 2 (sz,sz) RGBADepth (BackgroundRGBADepth (RGBA 0 1 1 0.6))
             , Allocate 3 (sz,sz) RGBADepth (BackgroundRGBADepth (RGBA 0 1 0 0.6))
             , Allocate 4 (sz,sz) RGBADepth (BackgroundRGBADepth (RGBA 1 0 1 0.6))
     	]] ++  
	     [	[
              SplatTriangle ((i + j) `mod` 4 + 1) 
			     0 (PointMap (1,1) (0.5,0.5)) 
			        (PointMap (1,1) (0.5 + scale * sin (fromIntegral j/10),0.5 + scale * cos (fromIntegral j/10)))
			        (PointMap (1,1) (0.5 + scale * sin (fromIntegral j/8),0.5 + scale * cos (fromIntegral j/8)))
	     | j <- [0..5000]
	     ] | i <- [0..]
	     ] , 0 )
  where scale = 0.2
        sz = 200
example2:: ([[Inst BufferId]], BufferId)
example2 = ( [ Allocate 0 (500,500) G8BitDepth (BackgroundG8Bit 0.8)
	     , AllocateImage 2 "back.jpg"
	     , Allocate 3 (1,1) RGBADepth (BackgroundG8Bit 0.8)
             , AllocateImage 6 "jhwk_RF_250px.gif"
	     , SplatPolygon 3 0 [(PointMap (0,0) (0,0)), (PointMap (1,0) (1,0)), (PointMap (1,1) (1,1)), (PointMap (0,1) (0,1))]
	     , OpenStream 7 (ffmpegOutCmd "test.mpeg")
             ] :
 	     [ [ SplatPolygon 2 0 [(PointMap (t0,t0) (0,0)), (PointMap (t1,t0) (1,0)), (PointMap (t1,t1) (1,1)), (PointMap (t0,t1) (0,1))]
	       , SplatPolygon 6 0 [(PointMap (0,0) (0+x,0.5 - y)), (PointMap (0,1) (0 + x,1 - y)), (PointMap (1,1) (0.5 + x,1 - y)), (PointMap (1,0) (0.5  + x,0.5 - y))]
	       , WriteStream 0 7
	       ]
	     | (x,y,z) <- zip3 (let t = take (50 * 5) [0,0.002..] in cycle (t ++ reverse t))
			       (let t = take (40 * 5) [0,0.0025..] in cycle (t ++ reverse t))
			       (take 100 (let t = take (40 * 20) [0,0.00025..] in cycle (t ++ reverse t)))
	     , let t0 = z
	     , let t1 = 1 - z
	     ]
	     ++ [[ 
             CloseStream 7
             ]]
           , 0
           )
           
example3 :: ([[Inst BufferId]], BufferId)
example3 = ( [[ AllocateImage 1 "back.jpg"
             ,  Allocate 2 (300,300) RGBADepth (BackgroundRGBADepth (RGBA 1 1 1 1))
             ,  SplatPolygon 1 2 [(PointMap (0,0) (0,0)), (PointMap (1,0) (1,0)), (PointMap (1,1) (1,1)), (PointMap (0,1) (0,1))]
             ,  SplatColor (RGBA 0 0 0 1) 2 True [(0.15,0.15), (0.145,0.145), (0.145,0.855), (0.15,0.85)]
             ,  SplatColor (RGBA 0 0 0 1) 2 True [(0.15,0.85), (0.145,0.855), (0.855,0.855), (0.85,0.85)]
             ,  SplatColor (RGBA 0 0 0 1) 2 True [(0.85,0.85), (0.855,0.855), (0.855,0.85), (0.8535,0.8465),
                                               (0.1535,0.1465), (0.15,0.145), (0.145,0.145), (0.15,0.15)]
             ,  SaveImage 2 "TestImage1.bmp"
	     ,  Allocate 3 (300,300) RGBADepth (BackgroundRGBADepth (RGBA 0.9 0 0 1))
	     ,  Allocate 0 (300,300) RGBADepth (BackgroundRGBADepth (RGBA 0 0 0 0.5))
	         ]] ++
	         [[  SplatColor (RGBA 0 0 0.9 1) 3 True [(x0,t0), (x1,t0), (x1,t1), (x0,t1)]
	            | t0 <- [0,0.1..0.9], let t1 = t0+0.1, x0 <- if ((round (t1*10))`mod`2==0) then [0,0.2..0.8] else [0.1,0.3..0.9], let x1 = x0+0.1
	         ]]
	         ++ [[
             --,  SplatPolygon 1 3 [(PointMap (0.25,0.25) (0.25,0.75)), (PointMap (0.25,0.75) (0.75,0.75)), (PointMap (0.75,0.75) (0.25,0.75))] -- to flip it
                SplatPolygon 1 3 [(PointMap (0.15,0.15) (0.85,0.85)), (PointMap (0.15,0.85) (0.85,0.15)), (PointMap (0.85,0.85) (0.15,0.15))]
             ,  SplatColor (RGBA 0 0 0 1) 3 True [(0.85,0.85), (0.855,0.855), (0.855,0.145), (0.85,0.15)]
             ,  SplatColor (RGBA 0 0 0 1)  3 True [(0.85,0.15), (0.855,0.145), (0.145,0.145), (0.15,0.15)]
             ,  SplatColor (RGBA 0 0 0 1) 3 True [(0.15,0.15), (0.145,0.145), (0.145,0.15), (0.1465,0.1535),
                                               (0.8465,0.8535), (0.85,0.855), (0.855,0.855), (0.85,0.85)]
             ,  SaveImage 3 "TestImage2.bmp"
             ,  CopyBuffer WithDestAlpha 3 0
             ,  SaveImage 0 "TestImage3.bmp"
             ,  SplatBuffer 0 0
             ,  SaveImage 0 "TestImage0.bmp"
             , OpenStream 7 (ffmpegOutCmd "test.avi")
             ]
             ++ [
             WriteStream 3 7 | x <- [0..100] ]
             ++ [ 
             CloseStream 7
             ]]
           , 0
           )
	          



--foreign import ccall "&" myptr :: Ptr CUChar
