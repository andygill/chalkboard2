module Main where
	
import Graphics.ChalkBoard as CB
--import Control.Applicative
import Data.Array
import System.Cmd
import System.Environment
import Graphics.ChalkBoard
--	(white,  black,  red,   green,  blue,  cyan,  purple,  yellow, RGB(..), RGBA(..))

import Graphics.ChalkBoard.CBIR as CBIR

-- A rather messy test, tries out various aspects of our compiler and render engine.

-- normal test
main = do
	args <- getArgs
	case args of
	  ["server"] -> do
		cb <- openChalkBoard [BoardSize 100 100]
		cbMain cb
	  _ -> startChalkBoard [BoardSize 100 100] $ cbMain

-- server test
--main = do cb <- openChalkBoard []
--          cbMain cb

colors :: [(O RGB,String)]
colors = zip 
	[white,  black,  red,   green,  blue,  cyan,  purple,  yellow]
	["white","black","red","green","blue","cyan","purple","yellow"]

cbMain cb = do
	-- hack to let us control what is tested
{-
	let test :: String -> [IO ()] -> IO ()
	    test "test9" = sequence_
--	    test _ = sequence_
	    test _       = \ xs -> return ()
-}
	
	let test _ = sequence_

	-- load an image to use for rotations, etc.
	(x,y,imgBrd) <- readBoard ("images/cb-text.png")
	let xy = fromIntegral $ max x y
	let sc = 1 / xy
	let xd = fromIntegral y / xy
	let yd = fromIntegral x / xy
	let img = unAlpha <$> move (-0.5 * yd,-0.5 * xd)  (scale sc imgBrd)

	-- first examples, pure colors.
	test "test1" 
	          [ do drawChalkBoard cb (boardOf col)
		       writeChalkBoard cb ("test1-" ++ nm ++ ".png")
		  | (col,nm) <- colors -- (cycle colors)
		  ]

	-- next, test basic shapes with rotations, scalings, etc.
        test "test2" [ do
	   	test "scale"
	 		  [ do drawChalkBoard cb (scale n shape)
		               writeChalkBoard cb ("test2-scale-" ++ shape_name ++ "-" ++ show n ++ ".png")
	                  | n <- [1,0.5]
		          ]
	   	test "circle"
	 		  [ do drawChalkBoard cb ((rotate r (scale 0.5 shape))
					 )
		               writeChalkBoard cb ("test2-rotate-" ++ shape_name ++ "-" ++ nm ++ ".png") 
	             	  | (r,nm) <- zip
				[0,0.1,-0.1,pi/10,pi,2*pi]
				["0","0.1","neg0.1","pi_div10","pi","2pi"]
		          ]
	   	test "move" 
		          [ do drawChalkBoard cb ((move (x,y) (scale 0.5 shape))
					 )
		               writeChalkBoard cb ("test2-move-" ++ shape_name ++ "-" ++ nmY ++ nmX ++ ".png")
	 	     	  | let amount = 0.25
		          , (x,nmX) <- [(-amount,"left"),(0,"center"),(amount,"right")]
	 	          , (y,nmY) <- [(amount,"top"),(0,"middle"),(-amount,"bottom")]
		          ]
		test "scale" 
		          [ do  drawChalkBoard cb (scaleXY (x,y) shape)
		                writeChalkBoard cb ("test2-scaleXY-" ++ shape_name ++ "_" ++ nmX ++ "_" ++ nmY ++ "_.png")
			  | let ranges =  [(1,"1"),(0.5,"0.5"),(0.1,"0.1"),(-0.1,"neg0.1")]
			  , (x,nmX) <- ranges
			  , (y,nmY) <- ranges
			  ]
		test "chain" [ do  
				drawChalkBoard cb ((f (scale 0.5 shape))
					 )
		                writeChalkBoard cb ("test2-chain-" ++ shape_name ++ "-" ++ chain ++ ".png")
			  | let ranges =  [(1,"1"),(0.5,"0.5"),(0.1,"0.1"),(-0.1,"neg0.1")]
			  , (f,chain) <- [ (move (0.2,0.2) . rotate 1, "move-after-rot")
					 , (rotate 1 . move (0.2,0.2), "rot-after-move")
					 , (move (0.2,0.2) . scale 0.9, "move-after-scale")
					 , (scale 0.9 . move (0.2,0.2), "scale-after-move")
					 ]
			  ]
			
            | (shape,shape_name) <-  
				    [  (choose (red) (white) <$> square,"square")
				    , (choose (blue) (white) <$> circle,"circle") 
				    ,  (img,"img")
				    , (choose (green) (white) <$> triangle (-0.5,-0.5) (0.5,-0.5) (0,0.5),"triangle")
				    ]
            ]

	-- load an image; display it.
	test "test3" [ do
		(x,y,imgBrd) <- readBoard ("images/" ++ nm)
		let xy = max x y
		drawChalkBoard cb (unAlpha <$> move (-0.5,-0.5) (scale (1/fromIntegral xy) imgBrd))
		writeChalkBoard cb $ "test3-image-load-" ++ nm ++ ".png"
	   | nm <- [ "cb-text.gif"
		   , "cb-text.jpg"
		   , "cb-text.png"
		   ] 
	   ]

	test "test4" [ do 
		       let r = move (0.26,0.15)  (choose (withAlpha a red) (transparent white) <$> circle)
	                   g = move (-0.26,0.15) (choose (withAlpha a green) (transparent white) <$> circle)
	                   b = move (0,-0.3)      (choose (withAlpha a blue) (transparent white) <$> circle)
		       drawChalkBoard cb (scale 0.5 (unAlpha <$> (r `over` b `over` g `over` boardOf (transparent white))))
		       writeChalkBoard cb $ "test4-" ++ show a ++  ".png"
		 | a <- [0,0.1,0.3,0.5,0.7,0.9,1]
		 ]

	-- These should be a single color,
	-- and not bleed through each other

	test "test5" [ 
		sequence_ [ do 
		       let r = move (0.26,0.15) circle
	                   g = move (-0.26,0.15) circle
	                   b = move (0,-0.3)    circle
		       drawChalkBoard cb (scale 0.5 (unAlpha <$> 
							(choose (withAlpha a green) (transparent white) <$>
							    (r `over` b `over` g))))
		       writeChalkBoard cb $ "test5-" ++ show a ++  ".png" ]
		 | a <- [0,0.1,0.3,0.5,0.7,0.9,1]
		 ]

-- This should show a single shape of overlap between the two snowmen.

	let overX a b = a `over` b
	test "test6" [ 
		sequence_ [ do
		       let rs0 = [ move (i * 0.26,j * 0.26) circle
			        | i <- [-1,1], j <- [-1,1]
			        ]
		       let rs = [ scale i b | (i,b) <- zip [1,0.9..] rs0 ]
		       drawChalkBoard cb (scale 0.5 (unAlpha <$> 
							((choose (withAlpha a green) 
								(transparent white) <$>
							    ((rs !! 0) `over` (rs !! 1))) `overX`
							 (choose (withAlpha a red) 
								(transparent white) <$>
							    ((rs !! 2) `over` (rs !! 3))))))
		       writeChalkBoard cb $ "test6-" ++ show a ++  ".png" ]
		 | a <- [0,0.5,0.7,0.9,1]
		 ]

	test "test7" [ do
		buff <- readBuffer ("images/" ++ nm)
		let ((0,0),(x,y)) = bufferBounds buff
		let xy = max (x+1) (y+1)
		-- draw buffer board
		drawChalkBoard cb (unAlpha <$> move (-0.5,-0.5) (scale (1/fromIntegral xy) (bufferOnBoard buff $ boardOf (alpha green))))
		writeChalkBoard cb $ "test7-image-load-" ++ nm ++ "-as-Board.png"
		drawChalkBuffer cb (unAlpha <$> buff)
		writeChalkBoard cb $ "test7-image-load-" ++ nm ++ "-as-Buffer.png"
	   | nm <- [ "cb-text.gif"
		   , "cb-text.jpg"
		   , "cb-text.png"
		   ] 
	   ]

	test "test8" [ do
		buff <- readBuffer ("images/cb-text.png")
		let ((0,0),(x,y)) = bufferBounds buff
		let xy = max (x+1) (y+1)
		let brd = bufferOnBoard buff $ boardOf (alpha red)
		drawChalkBoard cb (unAlpha <$> move (-0.5,-0.5) (scale (1/fromIntegral xy) brd))
		writeChalkBoard cb $ "test8-0.png"
		let buff2 = boardToBuffer (0,0) (150,70) brd
		let brd2 = bufferOnBoard buff2 $ boardOf (alpha green)
		drawChalkBoard cb (unAlpha <$> move (-0.5,-0.5) (scale (1/fromIntegral xy) brd2))
		writeChalkBoard cb $ "test8-1.png"
	   ]

	test "test9" [ do
		let brd1 = scale 0.5 $ rotate 0.1 $ (choose red green <$> square)
		let brd2 = scale 0.9 $ choose yellow blue <$> square
		let brd3 = scale n $ circle
		let fn :: O ((RGB,RGB),Bool) -> O RGB
		    fn o = choose (fstO (fstO o)) (sndO (fstO o)) (sndO o)
		drawChalkBoard cb $ fn <$> ((brd1 `ozip` brd2) `ozip` brd3)
		writeChalkBoard cb $ "test9-" ++ show n ++ ".png"
	    | n <- [0.1,0.5,1.0,2.0]
	    ]
	
	exitChalkBoard cb


{-
	test "test9" [ do
		drawRawChalkBoard cb (example1 (x,y))
		writeChalkBoard cb $ "test9-" ++ show x  ++ "-" ++ show y ++ ".png"
	    | (x,y) <- [(150,150),(100,100),(200,200)]
	    ]


-- example of fBO size probleme
example1 sz = 
     [ Allocate 1004 (150,150) RGBADepth (BackgroundRGBADepth $ RGBA 1.0 1.0 1.0 1.0)
     , SaveImage 1004 "test9-1.png"
     , SplatColor (RGBA 0.0 1.0 0.0 1.0) 1004 False [(0.0,0.0),(0.0,1.0),(1.0,1.0),(1.0,0.0)]
     , SaveImage 1004 "test9-2.png"

     ,  Allocate 1005 sz RGBADepth (BackgroundRGBADepth $ RGBA 0.0 0.0 1.0 1.0)
     , SaveImage 1005 "test9-3.png"
     , SplatColor (RGBA 1.0 0.0 0.0 1.0) 1005 False [(0.0,0.0),(0.0,1.0),(1.0,1.0),(1.0,0.0)]
     , SaveImage 1005 "test9-4.png"

     , SplatPolygon 1005 1004 [PointMap (0.0,0.0) (0.0,0.0),PointMap (1.0,0.0) (1.0,0.0),PointMap (1.0,1.0) (1.0,0.25),PointMap (0.0,1.0) (0.0,0.25)]
     , SaveImage 1004 "test9-5.png"
     , CopyBuffer WithSrcAlpha 1004 0
     , SaveImage 0 "test9-6.png"

     ]

-}	
