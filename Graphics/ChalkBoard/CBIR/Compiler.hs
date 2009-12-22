{-# LANGUAGE TypeFamilies, FlexibleInstances, ExistentialQuantification, RankNTypes
  #-}
module Graphics.ChalkBoard.CBIR.Compiler where

-- The idea is to compiler ChalkBoard specs into CBIR instructions.
-- No idea how it will work, only that it will work. So here goes...
-- Quite scrappy, but will clean up.

import Graphics.ChalkBoard.Board
import Graphics.ChalkBoard.Types as Ty

import Graphics.ChalkBoard.O as O
import Graphics.ChalkBoard.O.Internals as OI
import Graphics.ChalkBoard.Core as C
import Graphics.ChalkBoard.Board as B
import Graphics.ChalkBoard.Internals as BI
import Graphics.ChalkBoard.CBIR as CBIR
import Data.Unique
import Data.Reify.Graph
import Graphics.ChalkBoard.Expr as Expr
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.IO
import Control.Monad
import Graphics.ChalkBoard.IStorable as IS
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS


import Unsafe.Coerce
import Debug.Trace

-- We always compile  a RGB board.
compile :: (Int,Int) -> BufferId -> Board RGB -> IO [CBIR.Inst Int]
compile (x,y) bufferId brd = 
	compileBoard compileBoardRGB (initBoardContext (x,y) bufferId) brd

-- Unless we are compiling a Buffer :-)
-- (Make this call compile)

compileB :: (Int,Int) -> BufferId -> Buffer RGB -> IO [CBIR.Inst Int]
compileB (x,y) bufferId buff = do
	compileBoard compileBoardRGB (initBoardContext (x,y) bufferId) (scaleXY (1/fromIntegral x,1/fromIntegral y) (BufferOnBoard buff (boardOf white)))
--	compileInsideBufferRGB (initBoardContext (x,y) bufferId) low high raw




mapPoint :: [Trans] -> (R,R) -> (R,R)
mapPoint [] 			(x,y) = (x,y)
mapPoint (Move (xd,yd) : r) 	(x,y) = mapPoint r (x + xd,y + yd)
mapPoint (Scale (xn,yn) : r) 	(x,y) = mapPoint r (x * xn,y * yn)
mapPoint (Rotate theta : r) 	(x,y) = mapPoint r ( cos (-theta) * x - sin (-theta) * y
						   , sin (-theta) * x + cos (-theta) * y
						   )

-- n+1 attempt
initBoardContext (x,y) i = BoardContext [] (x,y) i
data BoardContext = BoardContext 
	{ bcTrans :: [Trans]		-- movement of board
	, bcSize :: (Int,Int)		-- approx size of resolution required for final board
	, bcDest :: BufferId		-- board to draw onto
	}
	deriving Show
	
updateTrans :: Trans -> BoardContext -> BoardContext 
updateTrans mv bc = bc { bcTrans = mv : bcTrans bc }

data DrawWith = DrawWithColor   RGBA Bool		-- Draw with a alpha-ized color, Bool = if blending
	      | SplatFromBuffer BufferId	-- Splat from a specific Buffer
-- LATER	      | DrawWithTrue			-- just an on/off

drawWiths :: [DrawWith] -> BufferId -> (Point -> Point) -> [Point] -> [Inst BufferId]
drawWiths []       _    _ _     = []
drawWiths (dw:dws) dest f nodes = drawWith dw dest f nodes : drawWiths dws dest f nodes

drawWith :: DrawWith -> BufferId -> (Point -> Point) -> [Point] -> Inst BufferId
drawWith (DrawWithColor argb bld) dest f nodes  = 
	SplatColor (RGBA r g b a) dest bld (map f nodes)
  where
	(RGBA r g b a) = argb
-- Strange, we are always going from the same point to the same point?
-- We use to have (x,y) (f (x,y))

drawWith (SplatFromBuffer bid) dest f nodes = 
	SplatPolygon bid dest [ PointMap (f (x,y)) (f (x,y))
		              | (x,y) <- nodes
			      ]


{- We have a different compileBoard for each of the 'n' supported types
 -- Right now, this is
 --  * Bool
 --  * RGB
 --  * RGBA
 -- TO BE DONE:
 --  * UI		-- between 0 and 1, gray map
 --  * (R,R)		-- coord
 -- 
 -}
-- All these generic rewrites are head-type preserving.
-- You can assume that trans has been removed, and that Fmap is normalized.

compileBoard :: (BoardContext -> Board a -> IO [Inst Int]) 
		-> BoardContext
		 -> Board a
		 -> IO [CBIR.Inst Int]
-- compileBoard _ _ brd | trace (show ("compileBoard",brd)) False = undefined
compileBoard f bc (Trans mv brd) 		= compileBoard f (updateTrans mv bc) brd
-- (perhaps) we should not do this, until we can handle general functions
--compileBoard f bc (Fmap g (Fmap h brd)) 	= compileBoard f bc (Fmap (g . h) brd)
compileBoard f bc (Fmap g (Trans mv brd)) 	= compileBoard f bc (Trans mv (Fmap g brd))
compileBoard f bc other          		= f bc other

-- | compilerBoardBool interpretes a boolean board in context,
--- either drawing using a specific color, or drawing using a predefined texture.

-- This means write onto the given destination board (either a RGB board,
-- or a ??? board) the result.

compileBoardBool :: [DrawWith]
		 -> BoardContext
		 -> Board a		-- Bool
		 -> IO [CBIR.Inst Int]
compileBoardBool dw bc (Fmap g brd)   = error "(fmap (..) brd) :: Board Bool is not (yet) supported"
compileBoardBool dw bc (Over fn above below) = do
	-- not quite right; assumes that the backing board is white.
	before <- compileBoard (compileBoardBool dw) bc below
	after  <- compileBoard (compileBoardBool dw) bc above
	return   [ Nested "over Bool" 
		   ( before ++
		     [Nested "`over` Bool" []] ++
		     after 
		   )
		 ]
compileBoardBool dw bc (Polygon nodes) = do
	return $ [ Nested ("precision factor = " ++ show res) 
		    (drawWiths dw (bcDest bc) 
		    	       (mapPoint (bcTrans bc))
			       (nodes res))
	  	 ]
  where
	[(x0,y0),(x1,y1),(x2,y2)] = map (mapPoint [ Scale (a,b) | Scale (a,b) <- bcTrans bc]) [(0,0),(1,0),(0,1)]
	res   = 1 + max (abs (fromIntegral x * (x0 - x1))) (abs (fromIntegral y * (y0 - y2)))
	(x,y)  = bcSize bc

compileBoardBool dw bc (PrimConst o) = 
	case (evalE $ runO0 o) of
	   Just (E (O_Bool True)) -> do
		return [ Nested ("Const (True :: Bool)") $
		    (drawWiths dw (bcDest bc) 
		    	       (mapPoint []) -- now sure abot this??
			       [(0,0),(0,1),(1,1),(1,0)])
	       	       ]
	   Just (E (O_Bool False)) ->	-- background *is* false
		return []
	   other -> error $ "pure a :: Board Bool, can not compute a, found " ++ show other

compileBoardBool _ _ brd = error $ show ("Bool",brd)


-- compileBoardRGBA has the effect of drawing the result *on top of* the target board,
-- which is always a RGBA. It is someone elses problem to figure out what this board
-- is originally.

compileBoardRGBA :: BoardContext
		 -> Board a
		 -> IO [CBIR.Inst Int]	
compileBoardRGBA bc (Over fn above below) = do
	-- Correct: by the updating semantics of compileBoardRGBA.
	before <- compileBoard compileBoardRGBA bc below
	after  <- compileBoard compileBoardRGBA bc above
	return   [ Nested "over RGBA" 
		   ( before ++
		     [Nested "`over` RGBA" []] ++
		     after 
		   )
		 ]
compileBoardRGBA bc (PrimConst o) = 
	case (evalE $ runO0 o) of
	   Just (E (O_RGBA (RGBA r g b 1))) -> do
		return [ Nested ("Const (a :: RGBA, a = 1)") $
	          	  [ colorBoard (RGB r g b) (bcDest bc) ]
	       	       ]	
	   Just (E (O_RGBA rgba)) -> do
		newBoard <- newNumber
		return [ Nested ("Const (a :: RGBA)") $
			  [ Allocate 
		        	newBoard 	   -- tag for this ChalkBoardBufferObject
        			(1,1)		   -- tiny board
        			RGBADepth          -- depth of buffer	
				(BackgroundRGBADepth rgba)
			  , copyBoard newBoard (bcDest bc) 
			  ]
	       	       ]	
	   other -> error $ "pure a :: Board RGBA, can not compute a, found " ++ show other
compileBoardRGBA bc (Fmap f other) = do
	case typeOfFun f of
	   FUN_TY (EXPR_TY BOOL_Ty) (EXPR_TY RGBA_Ty) -> do
		case (applyBool f True,applyBool f False) of
		  (Just (O_RGBA tRGBA@(RGBA r g b 0)),Just (O_RGBA fRGBA@(RGBA _ _ _ 0))) -> do
				-- silly case, both are transparent, so do ***NOTHING***
			return $ [ Nested ("Bool -> RGBA, where both values are transparent") [] ]
		  (Just (O_RGBA tRGBA@(RGBA r g b a)),Just (O_RGBA fRGBA@(RGBA _ _ _ 0))) -> do
			-- We take a copy of the back board.
			backBoard <- newNumber
			let overlap = perhapsOverlapBoardBool other


			let drawWith = (if overlap then [SplatFromBuffer backBoard] else []) ++ [DrawWithColor (RGBA r g b a) False]

				-- Write onto the *same* board, but with projections from the backBoard
				-- which is a snapshot of the board right now.
			rest <- compileBoard (compileBoardBool drawWith)  bc other
			return $ 
			     [ Allocate 
		        	backBoard 	   -- tag for this ChalkBoardBufferObject
        			(bcSize bc)	   -- we know size
        			RGBADepth          -- depth of buffer	
				(BackgroundRGBADepth (RGBA 0 0 0 0))
			     , copyBoard (bcDest bc) backBoard 
			     ] ++ rest ++ 
			     [ Delete backBoard
			     ]
		  other -> error $ "fmap (f :: Bool -> RGBA) brd, when f False is not transparent, is unsupported (a = " 
					++ show other
	   FUN_TY (EXPR_TY RGBA_Ty) (EXPR_TY RGBA_Ty) -> error $ "fmap (... :: RGBA -> RGBA) brd, unsupported fmap argument"
	   FUN_TY a b -> error $ "fmap (... :: " ++ show a ++ " -> " ++ show b ++ ") brd :: Board RGBA is not supported"
compileBoardRGBA bc (BufferOnBoard (Buffer (x0,y0) (x1,y1) buff) back) = do
	-- assume def is transparent!
	-- assume the size of the buffer is okay. How do we do this?
	let (x,y) = bcSize bc
	-- TODO!
	-- really, this is about 0 and 1, not x and y.
	let mv = Scale (fromIntegral (1 + x1-x0) / fromIntegral 1,
			fromIntegral (1 + y1-y0) / fromIntegral 1)
	back_code <- compileBoard compileBoardRGBA bc back
	(code,buffId) <- compileInsideBufferRGBA (x0,y0) (x1,y1) buff
	let tr = bcTrans (updateTrans mv bc)
--	print ((x,y),(x1-x0,y1-y0))
	return [ Nested "buffer inside board (RGBA)" $
			back_code ++ code ++
			[Nested (show ((x,y),bcTrans (updateTrans mv bc))) []] ++
			[ SplatPolygon buffId (bcDest bc) 
		    		[ PointMap (x,y) (mapPoint tr (x,y))
		    		| (x,y) <- [(0,0),(1,0),(1,1),(0,1)]
		    		]
			]
	       ]
compileBoardRGBA _ brd = error $ show ("RGBA",brd)


-- This means write onto the given destination board (always a RGB board) the result.
compileBoardRGB :: (BoardContext)
		-> Board a
		-> IO [CBIR.Inst Int]
compileBoardRGB bc (Over fn top bottom) = compileBoard compileBoardRGB bc top
compileBoardRGB bc (BI.PrimConst o) = 
	case (evalE $ runO0 o) of
	   Just (E (O_RGB (RGB r g b))) -> do
		return [ Nested ("Const (a :: RGB)") $
	          	  [ colorBoard (RGB r g b) (bcDest bc) ]
	       	       ]	
	   _ -> error "pure a :: Board RGB, can not compute a??"
compileBoardRGB bc (Fmap f other) = do
	fMapFn <- patternOf $ f
	case argTypeForFunX f RGB_Ty of
	   Just BOOL_Ty -> do
		backBoard <- newNumber
		frontBoard <- newNumber
--		print $ runToFind (O_Bool False) fMapFn
		let (O_RGB fcol) = runToFind (O_Bool False) fMapFn
		let (O_RGB tcol) = runToFind (O_Bool True) fMapFn
		let (RGB r g b)    = tcol
		let (RGB r' g' b') = fcol
--		print (fcol,tcol)
		let bc' = bc 
		rest <- compileBoard (compileBoardBool [DrawWithColor (RGBA r g b 1) False]) bc' other
		return [ Nested ("BOOL -> RGB") $
			 [ Allocate 
        			backBoard 	   -- tag for this ChalkBoardBufferObject
        			(1,1)		   -- we know size
        			RGB24Depth           -- depth of buffer
				(BackgroundRGB24Depth (RGB r' g' b'))
			, Allocate 
				frontBoard
        			(1,1)		   -- we know size
        			RGB24Depth           -- depth of buffer
				(BackgroundRGB24Depth (RGB r g b))
			, copyBoard backBoard (bcDest bc)
			] ++ rest ++ 
			[ Delete backBoard, Delete frontBoard]]
	   Just RGBA_Ty -> do
		-- turn rgb*ALPHA* thing, and translate this into a rgb.
		-- Assume unAlpha (for now)
		newBoard <- newNumber
		let bc' = bc
			 { bcDest = newBoard
  		         }
		rest <- compileBoard compileBoardRGBA bc' other
		return [ Nested ("RGBA -> RGB") $ 
			[ Allocate 
		        	newBoard 	   -- tag for this ChalkBoardBufferObject
        			(bcSize bc)		   -- we know size
        			RGBADepth           -- depth of buffer
				(BackgroundRGBADepth (RGBA 1 1 1 1))	-- ???
			] ++ rest ++
			[  copyBoard newBoard (bcDest bc)
			, Delete newBoard
		        ]]
{-
	   Just (Pair_Ty RGB_Ty RGB_Ty) -> do
	      let e = applyVar f
	      case (e,other) of
		 (Hook msg (E (Var 0)),Zip b1 b2) -> do
			-- turn rgb*ALPHA* thing, and translate this into a rgb.
		-- Assume unAlpha (for now)
			newBoard1 <- newNumber
			newBoard2 <- newNumber
			newFrag <- newNumber
			let bc' = bc { bcDest = newBoard1 }
			rest1 <- compileBoard compileBoardRGB bc' b1
			let bc' = bc { bcDest = newBoard2 }
			rest2 <- compileBoard compileBoardRGB bc' b2
			let (x0,x1,y0,y1) = (0.05,0.7,0.35,0.92)
			return [ Nested ("fmap magic") $ 
				[ Allocate 
		        		newBoard1	   -- tag for this ChalkBoardBufferObject
        				(bcSize bc)		   -- we know size
        				RGB24Depth           -- depth of buffer
					(BackgroundRGB24Depth (RGB 1 1 1))	-- ???
				] ++
				[ Allocate 
		        		newBoard2	   -- tag for this ChalkBoardBufferObject
        				(bcSize bc)		   -- we know size
        				RGB24Depth           -- depth of buffer
					(BackgroundRGB24Depth (RGB 1 1 1))	-- ???
				] ++ rest1 ++ rest2 ++
				[ AllocFragmentShader newFrag msg []
				, copyBoard newBoard1 (bcDest bc)
				, SplatWithFunction newFrag [newBoard1,newBoard2] (bcDest bc) [PointMap (x,y) (x,y) | (x,y) <- [(x0,y0),(x1,y0),(x1,y1),(x0,y1)]]
				, Delete newBoard1, Delete newBoard2
		        	]]
		 _ -> do print ""
			 error $ "fmap (...) :: Board RGB problem : " ++ show e
-}

	   a -> error $ "fmap (... :: " ++ show a ++ " -> " ++ show RGB_Ty ++ ") brd :: Board RGB is not supported"
compileBoardRGB bc (BufferOnBoard (Buffer (x0,y0) (x1,y1) buff) back) = do
	-- assume def is transparent!
	-- assume the size of the buffer is okay. How do we do this?
	let (x,y) = bcSize bc
	-- TODO!
	-- really, this is about 0 and 1, not x and y.
	let mv = Scale (fromIntegral (1 + x1-x0) / fromIntegral 1,
			fromIntegral (1 + y1-y0) / fromIntegral 1)
	back_code <- compileBoard compileBoardRGB bc back
	(code,buffId) <- compileInsideBufferRGB (x0,y0) (x1,y1) buff
	let tr = bcTrans (updateTrans mv bc)
--	print ((x,y),(x1-x0,y1-y0))
	return [ Nested "buffer inside board (RGB)" $
			back_code ++ code ++
			[ SplatPolygon buffId (bcDest bc) 
		    		[ PointMap (x,y) (mapPoint tr (x,y))
		    		| (x,y) <- [(0,0),(1,0),(1,1),(0,1)]
		    		]
			]
	       ]
compileBoardRGB bc (BoardGSI fn bargs vargs) = do
	let boards_RGB  = [ (nm,brd) | (nm,BoardRGBArgument brd) <- bargs ]
	let boards_Bool = [ (nm,brd) | (nm,BoardBoolArgument brd) <- bargs ]
	let boards_UI   = [ (nm,brd) | (nm,BoardUIArgument brd) <- bargs ]
 
	num_for_boards_RGB <- sequence [ newNumber | _ <- boards_RGB ]
	num_for_boards_Bool <- sequence [ newNumber | _ <- boards_Bool ]
	num_for_boards_UI <- sequence [ newNumber | _ <- boards_UI ]
	let create_Boards
	 	  = [ Allocate 
		        num		   -- tag for this ChalkBoardBufferObject
        		(bcSize bc)		   -- we know size
        		RGB24Depth           -- depth of buffer
			(BackgroundRGB24Depth (RGB 0 0 0))	-- black is the background, now! (== False)
		    | num <- num_for_boards_RGB ++ num_for_boards_Bool ++ num_for_boards_UI
		    ]
	fill_Boards_RGB
	 	<- sequence [ compileBoard compileBoardRGB (bc { bcDest = brdId }) brd
			    | ((_,brd),brdId) <- zip boards_RGB num_for_boards_RGB
			    ]
	fill_Boards_Bool
	 	<- sequence [ compileBoard (compileBoardBool [DrawWithColor (RGBA 1 1 1 1) False]) (bc { bcDest = brdId }) brd
			    | ((_,brd),brdId) <- zip boards_Bool num_for_boards_Bool
			    ]
	fill_Boards_UI
	 	<- sequence [ compileBoard compileBoardUI (bc { bcDest = brdId }) brd
			    | ((_,brd),brdId) <- zip boards_UI num_for_boards_UI
			    ]

	let delete_Boards
		  = [ Delete num 
	            | num <- num_for_boards_RGB ++ num_for_boards_Bool ++ num_for_boards_UI
	            ]

	newFrag <- newNumber
	
 	let (x0,x1,y0,y1) = (0,1,0,1) 
	return $ [ AllocFragmentShader newFrag fn [] ]
		++ create_Boards
		++ concat fill_Boards_RGB
		++ concat fill_Boards_Bool
		++ concat fill_Boards_UI
		++ [ SplatWithFunction newFrag 
				[ (nm,brdId) 
				| (nm,brdId) <- zip (map fst boards_RGB ++ map fst boards_Bool  ++ map fst boards_UI)
						    (num_for_boards_RGB ++ num_for_boards_Bool ++ num_for_boards_UI)
				]
				vargs
				(bcDest bc) 
				[PointMap (x,y) (x,y) | (x,y) <- [(x0,y0),(x1,y0),(x1,y1),(x0,y1)]]
		   ]
		++ delete_Boards

	
compileBoardRGB _ brd = error $ show ("compileBoardRGB",brd)


compileBoardUI bc (BufferOnBoard (Buffer (x0,y0) (x1,y1) buff) back) = do
	-- assume def is transparent!
	-- assume the size of the buffer is okay. How do we do this?
	let (x,y) = bcSize bc
	-- TODO!
	-- really, this is about 0 and 1, not x and y.
	let mv = Scale (fromIntegral (1 + x1-x0) / fromIntegral 1,
			fromIntegral (1 + y1-y0) / fromIntegral 1)
	back_code <- compileBoard compileBoardUI bc back
	(code,buffId) <- compileInsideBufferUI (x0,y0) (x1,y1) buff
	let tr = bcTrans (updateTrans mv bc)
--	print ((x,y),(x1-x0,y1-y0))
	return [ Nested "buffer inside board (UI)" $
			back_code ++ code ++
			[ SplatPolygon buffId (bcDest bc) 
		    		[ PointMap (x,y) (mapPoint tr (x,y))
		    		| (x,y) <- [(0,0),(1,0),(1,1),(0,1)]
		    		]
			]
	       ]
compileBoardUI bc (BI.PrimConst _) = 
--	case (evalE $ runO0 o) of
--	   Just (E (O_RGB (RGB r g b))) -> do
		return [ Nested ("Const (a :: UI)") $
	          	  [ colorBoard (RGB 0 0 0) (bcDest bc) ]
	       	       ]	
--	   _ -> error "pure a :: Board RGB, can not compute a??"compileBoardUI _ brd = error $ show ("compileBoardUI",brd)

compileInsideBufferRGBA 
		 :: (Int,Int)
		 -> (Int,Int)
		 -> InsideBuffer a
		 -> IO ([CBIR.Inst BufferId],BufferId)
compileInsideBufferRGBA low@(x0,y0) high@(x1,y1) (BoardInBuffer brd) = do
	newBoard <- newNumber
	let (sx,sy) = (1 + x1 - x0, 1 + y1 - y0)

	-- we want to map (x0,y0) x (x1,y1) onto the Board (0,0) x (1,1)
	let mv = Move (fromIntegral x0,fromIntegral y0)
	let sc = Scale (1 / fromIntegral sx,1 / fromIntegral sy)
	let bc = updateTrans mv $ updateTrans sc $ initBoardContext (sx,sy) newBoard
	rest <- compileBoard compileBoardRGBA bc brd
	return ( [ Nested ("BoardInBuffer") $
		   [ Allocate 
			newBoard 	-- tag for this ChalkBoardBufferObject
			(sx,sy)	   	-- we know size
			RGBADepth       -- depth of buffer
			(BackgroundRGBADepth (RGBA 1 1 1 1))
		   ] ++ rest
	         ], newBoard)
compileInsideBufferRGBA low high (ImageRGBA bs) = do
	compileByteStringImage low high bs RGBADepth

compileInsideBufferUI 
		 :: (Int,Int)
		 -> (Int,Int)
		 -> InsideBuffer a
		 -> IO ([CBIR.Inst BufferId],BufferId)
compileInsideBufferUI low high (ImageUI bs) = do
	compileByteStringImage low high bs' RGB24Depth
   where bs' = BS.concatMap (\ w -> BS.pack [w,0,0]) bs 
compileInsideBufferUI lo hi inside = error $ show ("compileInsideBufferUI",lo,hi,inside)

compileInsideBufferRGB 
		 :: (Int,Int)
		 -> (Int,Int)
		 -> InsideBuffer a
		 -> IO ([CBIR.Inst BufferId],BufferId)
compileInsideBufferRGB low high (ImageRGB bs) = do
	compileByteStringImage low high bs RGB24Depth
compileInsideBufferRGB low@(x0,y0) high@(x1,y1) (FmapBuffer f buff) = do
	let size =  (1+x1-x0,1+y1-y0)
	fMapFn <- patternOf $ f
	case argTypeForFunX f RGB_Ty of
	   Just RGBA_Ty -> do
		newBoard <- newNumber
		(rest,buffId) <- compileInsideBufferRGBA low high buff
		return ([ Nested ("RGBA -> RGB") $ 
			rest ++
			[ Allocate 
		        	newBoard 	   -- tag for this ChalkBoardBufferObject
        			size		   -- we know size
        			RGBADepth           -- depth of buffer
				(BackgroundRGB24Depth (RGB 1 1 1))	-- ???
			, copyBoard buffId newBoard
			, Delete buffId
		        ]],newBoard)
	   ans -> error $ "fmap (... :: " ++ show ans ++ " -> " ++ show RGB_Ty ++ ") brd :: Buffer RGB is not supported"
compileInsideBufferRGB low@(x0,y0) high@(x1,y1) (BoardInBuffer brd) = do
	newBoard <- newNumber
	let (sx,sy) = (1 + x1 - x0, 1 + y1 - y0)

	-- we want to map (x0,y0) x (x1,y1) onto the Board (0,0) x (1,1)
	let mv = Move (fromIntegral x0,fromIntegral y0)
	let sc = Scale (1 / fromIntegral sx,1 / fromIntegral sy)
	let bc = updateTrans mv $ updateTrans sc $ initBoardContext (sx,sy) newBoard
	rest <- compileBoard compileBoardRGB bc brd
	return ( [ Nested ("BoardInBuffer") $
		   [ Allocate 
			newBoard 	-- tag for this ChalkBoardBufferObject
			(sx,sy)	   	-- we know size
			RGB24Depth       -- depth of buffer
			(BackgroundRGB24Depth (RGB 1 1 1))
		   ] ++ rest
	         ], newBoard)			

-- compile image copies the relevent part of an image onto 
-- the back buffer, 1 pixel for 1 pixel, after applying transformations.
-- The first pixel inside an image array is the top-left pixel on the
-- screen (hence the 1-y, below).

compileByteStringImage 
	:: (Int,Int) -> (Int,Int) -> ByteString
	-> Depth 
	-> IO ([CBIR.Inst BufferId],BufferId)

compileByteStringImage low@(x0,y0) high@(x1,y1) bs depth = do
	newBoard <- newNumber
	let (maxx,maxy) = (1 + x1 - x0, 1 + y1 - y0)
	-- scale to fit
{-
	let mv = Scale (fromIntegral maxx / fromIntegral tx,
		       (fromIntegral maxy / fromIntegral ty))
-}
	return $ ([ Nested ("Image RGB create " ++ show (low,high)) 
		   [ Allocate 
			newBoard 	   -- tag for this ChalkBoardBufferObject
        		(maxx,maxy)		   -- we know size
        		depth           -- depth of buffer
			(BackgroundByteString bs)
{-
		   , SplatPolygon newBoard (bcDest bc) 
		    		[ PointMap (x,1-y) (mapPoint (bcTrans (updateTrans mv bc)) (x,y))
		    		| (x,y) <- [(0,0),(1,0),(1,1),(0,1)]
		    		]
-}
	  	   ]
	         ], newBoard)

compileImage 
	:: (Int,Int) -> (Int,Int) -> ReadOnlyCByteArray 
	-> Depth 
	-> IO ([CBIR.Inst BufferId],BufferId)

compileImage low@(x0,y0) high@(x1,y1) arr depth = do error "compileImage"
{-
	newBoard <- newNumber
	let (maxx,maxy) = (1 + x1 - x0, 1 + y1 - y0)
	-- scale to fit
{-
	let mv = Scale (fromIntegral maxx / fromIntegral tx,
		       (fromIntegral maxy / fromIntegral ty))
-}
	return $ ([ Nested ("Image RGB create " ++ show (low,high)) 
		   [ Allocate 
			newBoard 	   -- tag for this ChalkBoardBufferObject
        		(maxx,maxy)		   -- we know size
        		depth           -- depth of buffer
			(BackgroundArr arr)
{-
		   , SplatPolygon newBoard (bcDest bc) 
		    		[ PointMap (x,1-y) (mapPoint (bcTrans (updateTrans mv bc)) (x,y))
		    		| (x,y) <- [(0,0),(1,0),(1,1),(0,1)]
		    		]
-}
	  	   ]
	         ], newBoard)
-}



--compileBoardPair bc (Zip f b1 b) = do
--	case typeOfO f of
--	   foo -> error $ show ("Zip",foo)


-- Do you have overlapping shapes? Default to True, to be safe if do not know.	
perhapsOverlapBoardBool :: Board a -> Bool
perhapsOverlapBoardBool (Trans mv brd) = perhapsOverlapBoardBool brd
perhapsOverlapBoardBool (Polygon _)    = False	-- single polygon; no overlap
perhapsOverlapBoardBool _              = True


-- choice
patternOf :: (O a -> O b) -> IO (Graph Expr)
patternOf f = reifyO $ f (O (error "undefined shallow value") (E $ Var 1))


runToFind :: Expr E -> Graph Expr -> Expr E
runToFind arg (Graph nodes root) = eval (find root)
   where
	find i = case Prelude.lookup i nodes of
		    Just v -> v
		    Nothing -> error $ "can not find " ++ show i
	eval (Choose a b c) =
		case eval (find c) of
		  O_Bool True -> eval (find a)	-- false
		  O_Bool False -> eval (find b)	-- false
		  e -> error $ "expected a Bool, found something else " ++ show e
	eval (Var 1) = arg
	eval (O_RGB c) = O_RGB c

	eval (UnAlpha c) = case eval (find c) of
			    Expr.Alpha i (E c') -> c'
		  	    v -> UnAlpha (E v)
	eval (Expr.Alpha i c) = case eval (find c) of
			    UnAlpha (E c') | i == 1 -> c'
		  	    v -> Expr.Alpha i (E v)
	eval e = error $ "opps (eval) " ++ show e

-- Notice the lower case 'bool', because we are untyped in the compiler.
applyBool :: (O bool -> O a) -> Bool -> Maybe (Expr E)
applyBool f b = liftM unE (evalE (runO1 f (E $ O_Bool b)))

applyVar :: (O a -> O b) -> Expr E
applyVar f = unE (runO1 f (E $ Var 0))


newNumber :: IO Int
newNumber = do
	u <- newUnique
	return $ hashUnique u + 1000