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
	  _ -> startChalkBoard [BoardSize 100 100{-, DebugCBIR-}] $ cbMain

-- server test
--main = do cb <- openChalkBoard []
--          cbMain cb

colors :: [(O RGB,String)]
colors = Prelude.zip 
	[white,  black,  red,   green,  blue,  cyan,  purple,  yellow]
	["white","black","red","green","blue","cyan","purple","yellow"]

cbMain cb = do
	-- hack to let us control what is tested
{-
	let test :: String -> [IO ()] -> IO ()
	    test "test13" = sequence_
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
	let img = unAlphaBoard (boardOf black) $ move (-0.5 * yd,-0.5 * xd)  (scale sc imgBrd)

	-- test
	let brd = unAlphaBoard (boardOf white) $ choose (alpha green) (transparent) .$ circle
	drawChalkBoard cb brd
	writeChalkBoard cb ("testX.png")

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
	             	  | (r,nm) <- Prelude.zip
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
		drawChalkBoard cb (unAlphaBoard (boardOf white) $ move (-0.5,-0.5) (scale (1/fromIntegral xy) imgBrd))
		writeChalkBoard cb $ "test3-image-load-" ++ nm ++ ".png"
	   | nm <- [ "cb-text.gif"
		   , "cb-text.jpg"
		   , "cb-text.png"
		   ] 
	   ]

	test "test4" [ do 
		       let r = move (0.26,0.15)  (choose (withAlpha a red) (transparent) <$> circle)
	                   g = move (-0.26,0.15) (choose (withAlpha a green) (transparent) <$> circle)
	                   b = move (0,-0.3)      (choose (withAlpha a blue) (transparent) <$> circle)
		       drawChalkBoard cb (scale 0.5 (unAlphaBoard (boardOf white) $ (r `over` b `over` g)))
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
		       drawChalkBoard cb (scale 0.5 (unAlphaBoard (boardOf white) $ 
							((choose (withAlpha a green) (transparent) <$>
							    (r `over` b `over` g)))))
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
		       let rs = [ scale i b | (i,b) <- Prelude.zip [1,0.9..] rs0 ]
		       drawChalkBoard cb (scale 0.5 (unAlphaBoard (boardOf white) $ 
							((choose (withAlpha a green) 
								(transparent) <$>
							    ((rs !! 0) `over` (rs !! 1))) `overX`
							 (choose (withAlpha a red) 
								(transparent) <$>
							    ((rs !! 2) `over` (rs !! 3))))))
		       writeChalkBoard cb $ "test6-" ++ show a ++  ".png" ]
		 | a <- [0,0.5,0.7,0.9,1]
		 ]



	test "test7" [ do
		buff <- readBuffer ("images/" ++ nm)
		let ((0,0),(x,y)) = bufferBounds buff
		let xy = max (x+1) (y+1)
		-- draw buffer board
		drawChalkBoard cb (unAlphaBoard (boardOf white) $ move (-0.5,-0.5) (scale (1/fromIntegral xy) (bufferOnBoard buff $ boardOf (alpha green))))
		writeChalkBoard cb $ "test7-image-load-" ++ nm ++ "-as-Board.png"
-- TODO
--		drawChalkBuffer cb (unAlphaBoard (boardOf white) $ buff)
--		writeChalkBoard cb $ "test7-image-load-" ++ nm ++ "-as-Buffer.png"
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
		drawChalkBoard cb (unAlphaBoard (boardOf white) $ move (-0.5,-0.5) (scale (1/fromIntegral xy) brd))
		writeChalkBoard cb $ "test8-0.png"
		let buff2 = boardToBuffer (0,0) (150,70) brd
		let brd2 = bufferOnBoard buff2 $ boardOf (alpha green)
		drawChalkBoard cb (unAlphaBoard (boardOf white) $ move (-0.5,-0.5) (scale (1/fromIntegral xy) brd2))
		writeChalkBoard cb $ "test8-1.png"
	   ]

	test "test9" [ do
		let brd1 = scale 0.5 $ rotate 0.1 $ (choose red green <$> square)
		let brd2 = scale 0.9 $ choose yellow blue <$> square
		let brd3 = scale n $ circle
		let fn :: O ((RGB,RGB),Bool) -> O RGB
		    fn o = choose (fstO (fstO o)) (sndO (fstO o)) (sndO o)
		drawChalkBoard cb $ fn <$> ((brd1 `CB.zip` brd2) `CB.zip` brd3)
		writeChalkBoard cb $ "test9-" ++ show n ++ ".png"
	    | n <- [0.1,0.5,1.0,2.0]
	    ]

	test "test10" $ [ sequence_ $ 
	        [ do
                        drawChalkBoard cb $ unAlphaBoard (boardOf back) $ (boardOf (withAlpha alpha1 red)) `over` (boardOf (withAlpha alpha2 blue))
                        writeChalkBoard cb ("test10-2overs-" ++ show (alpha1,alpha2) ++ "-on-" ++ backTxt ++ ".png")
                  | alpha1 <- [0.0,0.5,1.0], alpha2 <- [0.0,0.5,1.0]
                ] ++
                [ do
                        drawChalkBoard cb $ unAlphaBoard (boardOf back) $ (boardOf (withAlpha alpha1 red)) `over` ((boardOf (withAlpha alpha2 blue)) `over` (boardOf (withAlpha alpha3 black)))
                        writeChalkBoard cb ("test10-3oversRight-" ++ show (alpha1,alpha2,alpha3) ++  "-on-" ++ backTxt ++ ".png")
                  | alpha1 <- [0.0,0.5,1.0], alpha2 <- [0.0,0.5,1.0], alpha3 <- [0.0,0.5,1.0]
                ] ++
                [ do
                        drawChalkBoard cb $ unAlphaBoard (boardOf back) $ ((boardOf (withAlpha alpha1 red)) `over` (boardOf (withAlpha alpha2 white))) `over` (boardOf (withAlpha alpha3 blue))
                        writeChalkBoard cb ("test10-3oversLeft-" ++ show (alpha1,alpha2,alpha3) ++  "-on-" ++ backTxt ++ ".png")
                  | alpha1 <- [0.0,0.5,1.0], alpha2 <- [0.0,0.5,1.0], alpha3 <- [0.0,0.5,1.0]
                ] ++
                [ do
                        drawChalkBoard cb $ unAlphaBoard (boardOf back) $ ((boardOf (withAlpha alpha1 white)) `over` (boardOf (withAlpha alpha2 red)))
                                                        `over`
                                                        ((boardOf (withAlpha alpha3 blue)) `over` (boardOf (withAlpha alpha4 yellow)))
                        writeChalkBoard cb ("test10-4overs-" ++ show (alpha1,alpha2,alpha3,alpha4) ++  "-on-" ++ backTxt ++ ".png")
                  | alpha1 <- [0.0,0.5,1.0], alpha2 <- [0.0,0.5,1.0], alpha3 <- [0.0,0.5,1.0], alpha4 <- [0.0,0.5,1.0]
                ]
            |  (back,backTxt) <- Prelude.zip [white,black] ["white","black"] ]
            
	test "test11" [ do 
		       let r = move (0.26,0.15)  (withMask red <$> circle)
	                   g = move (-0.26,0.15) (withMask green <$> circle)
	                   b = move (0,-0.3)     (withMask blue <$> circle)
		       drawChalkBoard cb (scale 0.5 (withDefault yellow <$> (r `over` b `over` g)))
		       writeChalkBoard cb $ "test11.png"
		 ]

	let xx :: O (Maybe UI) -> O RGB
	    xx = undefined 
		-- withDefault 0.0 

	test "test12" [ do 
		       let r, g, b :: Board (Maybe UI)
			   r = move (0.26,0.15)  (withMask 0.3 <$> circle)
	                   g = move (-0.26,0.15) (withMask 0.7 <$> circle)
	                   b = move (0,-0.3)     (withMask 0.9 <$> circle)
		       test "rot" [ do
		         drawChalkBoard cb (scale 0.5 (mix white black <$> (withDefault 0.0 <$> (c1 `over` c2 `over` c3))))
		         writeChalkBoard cb $ "test12-" ++ show i ++ ".png"
			| (i,c1:c2:c3:[]) <- Prelude.zip [0..] (permute [r,g,b])
			]
		]

	test "test13" [ do 
		       let r, g, b :: Board UI
			   r = move (0.26,0.15)  (choose 0.3 0.0 <$> circle)
	                   g = move (-0.26,0.15) (choose 0.5 0.0 <$> circle)
	                   b = move (0,-0.3)     (choose 0.7 0.0 <$> circle)
		       test "rot" [ do
		         drawChalkBoard cb (scale 0.5 (mix white black <$> (c1 `over` c2 `over` c3)))
		         writeChalkBoard cb $ "test13-" ++ show i ++ ".png"
			| (i,c1:c2:c3:[]) <- Prelude.zip [0..] (permute [r,g,b])
			]

		]

	exitChalkBoard cb


permute [] = [[]]
permute (x:xs) = concat (map (insert x) (permute xs))
     where
	insert x [] = [[x]]
	insert x (y:ys) = (x:y:ys) : map (\z -> y:z) (insert x ys)


example = 
 [ Nested "alloc BOOL_Ty"
     [ Allocate 1001 (100,100) RGB24Depth (BackgroundRGB24Depth $ RGB 0.0 0.0 0.0)
     , Nested "precision factor = 51.0"
         [ Splat 1001 Copy (SplatColor' (RGBA 1.0 1.0 1.0 1.0) [(0.37,0.825),(0.4007221,0.8231051),(0.43097842,0.8174492),(0.46031043,0.80811805),(0.48827338,0.79525304),(0.51444346,0.7790492),(0.5384239,0.7597522),(0.5598512,0.73765457),(0.5784006,0.71309125),(0.5937908,0.6864346),(0.6057886,0.6580887),(0.6142121,0.6284833),(0.61893356,0.59806705),(0.61988145,0.5673012),(0.61704135,0.5366521),(0.6104564,0.5065842),(0.6002264,0.47755352),(0.58650637,0.45),(0.5695043,0.42434132),(0.549478,0.40096653),(0.52673095,0.38022986),(0.501608,0.3624457),(0.4744901,0.3478837),(0.44578817,0.3367645),(0.41593733,0.3292567),(0.38539016,0.32547417),(0.35460973,0.32547417),(0.3240626,0.32925674),(0.2942118,0.3367645),(0.26550987,0.3478837),(0.23839194,0.36244574),(0.21326903,0.3802299),(0.19052202,0.40096653),(0.17049563,0.4243414),(0.15349364,0.45000002),(0.1397736,0.4775536),(0.1295436,0.5065843),(0.12295863,0.53665215),(0.12011856,0.5673013),(0.12106645,0.59806716),(0.12578791,0.6284833),(0.13421142,0.6580888),(0.14620918,0.6864346),(0.16159946,0.7130913),(0.18014878,0.73765457),(0.20157614,0.7597523),(0.22555655,0.7790492),(0.25172666,0.7952531),(0.2796897,0.8081181),(0.30902162,0.8174492),(0.33927804,0.82310516)])
         ]
     ]
 , AllocFragmentShader 1002 "uniform sampler2D cb_sampler0;\nvec4 cb_Alpha(float a,vec3 x) { return vec4(x.r,x.g,x.b,a); }\nvec3 cb_UnAlpha(vec4 x) { return vec3(x.r,x.g,x.b) * x.a; }\nvec4 cb_WithMaskRGB(vec3 c,bool x) { return mix(vec4(0.0,0.0,0.0,0.0),vec4(c.r,c.g,c.b,1.0),x ? 1.0 : 0.0); }\nvec3 cb_WithDefaultRGB(vec3 c1,vec4 c2) { return mix(c1,c2.rgb,c2.a); }\nvec4 cb_WithMaskUI(float c,bool x) { return mix(vec4(0.0,0.0,0.0,0.0),vec4(c,0.0,0.0,1.0),x ? 1.0 : 0.0); }\nfloat cb_WithDefaultUI(float c1,vec4 c2) { return mix(c1,c2.r,c2.a); }\nvoid main(void) {\n  gl_FragColor.rgb = mix(vec3(1.0,1.0,1.0),vec3(0.0,0.0,0.0),mix(0.0,0.5,((texture2D(cb_sampler0,gl_TexCoord[0].st)).r > 0.5) ? 1.0 : 0.0));\n  gl_FragColor.a = 1.0;\n}\n" []
 , Splat 0 Copy (SplatFunction' 1002 [("cb_sampler0",1001)] [] [(0.0,0.0),(1.0,0.0),(1.0,1.0),(0.0,1.0)])
 , Delete 1002
 , Delete 1001
 ]
	
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
