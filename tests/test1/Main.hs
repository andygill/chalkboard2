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
	-- load an image to use for rotations, etc.
	(x,y,imgBrd) <- readBoard ("images/cb-text.png")
--	(x,y,imgBrd) <- readFunnyBoard
	let xy = fromIntegral $ max x y
	let sc = 1 / xy
	let xd = fromIntegral y / xy
	let yd = fromIntegral x / xy
	let img = unAlpha <$> move (-0.5 * yd,-0.5 * xd)  (scale sc imgBrd)

	-- first examples, pure colors.
	sequence_ [ do drawChalkBoard cb (boardOf col)
		       writeChalkBoard cb ("test1-" ++ nm ++ ".png")
		  | (col,nm) <- colors
		  ]
	-- next, test basic shapes with rotations, scalings, etc.
        sequence_ [ do
	   	sequence_ [ do drawChalkBoard cb (scale n shape)
		               writeChalkBoard cb ("test2-scale-" ++ shape_name ++ "-" ++ show n ++ ".png")
	                  | n <- [1,0.5]
		          ]
	   	sequence_ [ do drawChalkBoard cb ((rotate r (scale 0.5 shape))
					 )
		               writeChalkBoard cb ("test2-rotate-" ++ shape_name ++ "-" ++ nm ++ ".png")
	             	  | (r,nm) <- zip
				[0,0.1,-0.1,pi/10,pi,2*pi]
				["0","0.1","neg0.1","pi_div10","pi","2pi"]
		          ]
	   	sequence_ [ do drawChalkBoard cb ((move (x,y) (scale 0.5 shape))
					 )
		               writeChalkBoard cb ("test2-move-" ++ shape_name ++ "-" ++ nmY ++ nmX ++ ".png")
	 	     	  | let amount = 0.25
		          , (x,nmX) <- [(-amount,"left"),(0,"center"),(amount,"right")]
	 	          , (y,nmY) <- [(amount,"top"),(0,"middle"),(-amount,"bottom")]
		          ]
		sequence_ [ do  drawChalkBoard cb ((scaleXY (x,y) shape)
					 )
		                writeChalkBoard cb ("test2-scaleXY-" ++ shape_name ++ "_" ++ nmX ++ "_" ++ nmY ++ "_.png")
			  | let ranges =  [(1,"1"),(0.5,"0.5"),(0.1,"0.1"),(-0.1,"neg0.1")]
			  , (x,nmX) <- ranges
			  , (y,nmY) <- ranges
			  ]
		sequence_ [ do  drawChalkBoard cb ((f (scale 0.5 shape))
					 )
		                writeChalkBoard cb ("test2-chain-" ++ shape_name ++ "-" ++ chain ++ ".png")
			  | let ranges =  [(1,"1"),(0.5,"0.5"),(0.1,"0.1"),(-0.1,"neg0.1")]
			  , (f,chain) <- [ (move (0.2,0.2) . rotate 1, "move-after-rot")
					 , (rotate 1 . move (0.2,0.2), "rot-after-move")
					 , (move (0.2,0.2) . scale 0.9, "move-after-scale")
					 , (scale 0.9 . move (0.2,0.2), "scale-after-move")
					 ]
			  ]
			
            | (shape,shape_name) <- [  (choose (red) (white) <$> square,"square")
				    , (choose (blue) (white) <$> circle,"circle")
				    ,  (img,"img")
				    , (choose (green) (white) <$> triangle (-0.5,-0.5) (0.5,-0.5) (0,0.5),"triangle")
				    ]
            ]

	-- load an image; display it.
	sequence_ [ do
		(x,y,imgBrd) <- readBoard ("images/" ++ nm)
		let xy = max x y
		drawChalkBoard cb (unAlpha <$> move (-0.5,-0.5) (scale (1/fromIntegral xy) imgBrd))
		writeChalkBoard cb $ "test3-image-load-" ++ nm ++ ".png"
	   | nm <- [ "cb-text.gif"
		   , "cb-text.jpg"
		   , "cb-text.png"
		   ] 
	   ]

	sequence_ [ do let r = move (0.26,0.15)  (choose (withAlpha a red) (transparent white) <$> circle)
	                   g = move (-0.26,0.15) (choose (withAlpha a green) (transparent white) <$> circle)
	                   b = move (0,-0.3)      (choose (withAlpha a blue) (transparent white) <$> circle)
		       drawChalkBoard cb (scale 0.5 (unAlpha <$> (r `over` b `over` g `over` boardOf (transparent white))))
		       writeChalkBoard cb $ "test4-" ++ show a ++  ".png"
		 | a <- [0,0.5,0.7,0.9,1]
		 ]

	-- These should be a single color,
	-- and not bleed through each other

	sequence_ [ do let r = move (0.26,0.15) circle
	                   g = move (-0.26,0.15) circle
	                   b = move (0,-0.3)    circle
		       drawChalkBoard cb (scale 0.5 (unAlpha <$> 
							(choose (withAlpha a green) (transparent white) <$>
							    (r `over` b `over` g))))
		       writeChalkBoard cb $ "test5-" ++ show a ++  ".png"
		 | a <- [0,0.5,0.7,0.9,1]
		 ]

-- This should show a single shape of overlap between the two snowmen.

	sequence_ [ do let rs0 = [ move (i * 0.26,j * 0.26) circle
			        | i <- [-1,1], j <- [-1,1]
			        ]
		       let rs = [ scale i b | (i,b) <- zip [1,0.9..] rs0 ]
		       drawChalkBoard cb (scale 0.5 (unAlpha <$> 
							((choose (withAlpha a green) 
								(transparent white) <$>
							    ((rs !! 0) `over` (rs !! 1))) `over`
							 (choose (withAlpha a red) 
								(transparent white) <$>
							    ((rs !! 2) `over` (rs !! 3))))))
		       writeChalkBoard cb $ "test6-" ++ show a ++  ".png"
		 | a <- [0,0.5,0.7,0.9,1]
		 ]

	sequence_ [ do
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

	exitChalkBoard cb
