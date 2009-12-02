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
import Graphics.ChalkBoard.Board.Internals as BI
import Graphics.ChalkBoard.CBIR as CBIR
import Data.Unique
import Data.Reify.Graph
import Graphics.ChalkBoard.Expr as Expr
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.IO
import Control.Monad
import Graphics.ChalkBoard.IStorable as IS

import Unsafe.Coerce
import Debug.Trace

-- We always compile  a RGB board.
compile :: (Int,Int) -> BufferId -> Board RGB -> IO [CBIR.Inst Int]
compile (x,y) bufferId brd = compileBoard compileBoardRGB (initBoardContext (x,y) bufferId) brd

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
	, bcDest :: BufferId			-- board to draw onto
	}
	
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
compileBoardRGBA bc (Image arr) = do
	let ((0,0,0), (maxy,maxx,3)) = IS.bounds arr
	let moves = [Scale (fromIntegral (maxx+1),fromIntegral (maxy+1))] ++ bcTrans bc
	newBoard <- newNumber
	return $ [ Nested ("Image create " ++ show (maxx,maxy)) 
		   [ Allocate 
			newBoard 	   -- tag for this ChalkBoardBufferObject
        		(maxx+1,maxy+1)		   -- we know size
        		RGBADepth           -- depth of buffer
			(BackgroundArr arr)
		   , SplatPolygon newBoard (bcDest bc) 
		    		[ PointMap (x,y) (mapPoint moves (x,y))
		    		| (x,y) <- [(0,0),(1,0),(1,1),(0,1)]
		    		]
	  	   ]
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
	case typeOfFun f of
	   FUN_TY (EXPR_TY BOOL_Ty) (EXPR_TY RGB_Ty) -> do
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
	   FUN_TY (EXPR_TY RGBA_Ty) (EXPR_TY RGB_Ty) -> do
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
	   FUN_TY a b -> error $ "fmap (... :: " ++ show a ++ " -> " ++ show b ++ ") brd :: Board RGB is not supported"
compileBoardRGB _ brd = error $ show ("RGB",brd)


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