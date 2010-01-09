{-# LANGUAGE TypeFamilies, FlexibleInstances, ExistentialQuantification, RankNTypes, GADTs
  #-}
module Graphics.ChalkBoard.CBIR.Compiler where

-- The idea is to compiler ChalkBoard specs into CBIR instructions.
-- No idea how it will work, only that it will work. So here goes...
-- Quite scrappy, but will clean up.

import Graphics.ChalkBoard.Types as Ty

import Graphics.ChalkBoard.O as O
import Graphics.ChalkBoard.O.Internals as OI
import Graphics.ChalkBoard.Core as C
import Graphics.ChalkBoard.Board as B hiding (zip)
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
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List

import qualified Data.ByteString as BS


import Unsafe.Coerce
import Debug.Trace

-- We always compile  a RGB board.
compile :: (Int,Int) -> BufferId -> Board RGB -> IO [CBIR.Inst Int]
compile (x,y) bufferId brd = 
	compileBoard2 (initBoardContext (x,y) bufferId) Target_RGB brd 
--	compileBoard compileBoardRGB (initBoardContext (x,y) bufferId) brd

-- Unless we are compiling a Buffer :-)
-- (Make this call compile)

compileB :: (Int,Int) -> BufferId -> Buffer RGB -> IO [CBIR.Inst Int]
compileB (x,y) bufferId buff = do
	compileBoard2 (initBoardContext (x,y) bufferId) Target_RGB 
		(scaleXY (1/fromIntegral x,1/fromIntegral y) (Board RGB_Ty (BufferOnBoard buff (boardOf white))))


mapPoint :: [Trans] -> (R,R) -> (R,R)
mapPoint [] 			(x,y) = (x,y)
mapPoint (Move (xd,yd) : r) 	(x,y) = mapPoint r (x + xd,y + yd)
mapPoint (Scale (xn,yn) : r) 	(x,y) = mapPoint r (x * xn,y * yn)
mapPoint (Rotate theta : r) 	(x,y) = mapPoint r ( cos (-theta) * x - sin (-theta) * y
						   , sin (-theta) * x + cos (-theta) * y
						   )

-- Assumping that the init board is RGB.

initBoardContext (x,y) i = BoardContext [] (x,y) i Copy

data BoardContext = BoardContext 
	{ bcTrans :: [Trans]		-- movement of board
	, bcSize :: (Int,Int)		-- approx size of resolution required for final board
	, bcDest :: BufferId		-- board to draw onto
	, bcBlend :: Blender		-- how to draw onto the board
	}
	deriving Show
	
updateTrans :: Trans -> BoardContext -> BoardContext 
updateTrans mv bc = bc { bcTrans = mv : bcTrans bc }


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

-- Do you have overlapping shapes? Default to True, to be safe if do not know.	
perhapsOverlapBoardBool :: Board a -> Bool
perhapsOverlapBoardBool (Board _ (Trans mv brd)) = perhapsOverlapBoardBool brd
perhapsOverlapBoardBool (Board _ (Polygon _))    = False	-- single polygon; no overlap
perhapsOverlapBoardBool _              		 = True


-- choice
--patternOf :: (O a -> O b) -> IO (Graph Expr)
--patternOf f = reifyO $ f (O (error "undefined shallow value") (E $ Var []))

-- Notice the lower case 'bool', because we are untyped in the compiler.
--applyBool :: (O bool -> O a) -> Bool -> Maybe (Expr E)
--applyBool f b = liftM unE (evalE (runO1 f (E $ O_Bool b)))

--applyVar :: (O a -> O b) -> Expr E
--applyVar f = unE (runO1 f (E $ Var []))


newNumber :: IO Int
newNumber = do
	u <- newUnique
	return $ hashUnique u + 1000
	
	
	
------------------------------------------------------------------------------------------
-- Another attempt to unify the compiler.

-- 	Data		Alpha?	Notes

data Target 
 = Target_RGBA Blender --		Write => the type of blending writing to a board does
 | Target_RGB      --	N	A = 1
 | Target_Bool RGB --	N	Arg is what do we draw for *True*
 | Target_Maybe_RGB --	Y	Nothing => transparent <ANY>
		   --  		works if * All transparent boards are considered equal
		   --			 * RGBA 0 0 0 0 is the unit
 | Target_UI
 | Target_Maybe_UI	-- 	use r and a{0,1}.
	deriving Show


-- What 'over' do with the forground and background?
targetOverBlend :: Target -> Blender -> ( Blender, Maybe Blender )
targetOverBlend (Target_UI)  Copy     = ( Max, Just $ Copy )
targetOverBlend (Target_UI)  Max      = ( Max, Just $ Max ) 
targetOverBlend (Target_UI)  Blend    = ( Max, Just $ Max ) 	-- Err, should never happen
targetOverBlend (Target_RGB) Copy       = ( Copy, Nothing )
targetOverBlend (Target_RGBA _) Copy    = ( Blend, Just $ Copy ) 
targetOverBlend (Target_RGBA _) Blend   = ( Blend, Just $ Blend ) 
targetOverBlend (Target_Bool c) Blend = ( Blend, Just $ Blend )
targetOverBlend (Target_Bool c) Copy  = ( Blend, Just $ Copy )
targetOverBlend (Target_Bool c) Max   = ( Blend, Just $ Max )
targetOverBlend (Target_Maybe_RGB) Blend = ( Blend, Just $ Blend )
targetOverBlend (Target_Maybe_RGB) Copy  = ( Blend, Just $ Copy )
targetOverBlend (Target_Maybe_UI) Blend  = ( Blend, Just $ Blend )
targetOverBlend (Target_Maybe_UI) Copy   = ( Blend, Just $ Copy )
targetOverBlend other b = error $ show ("targetOverBlend",other,b)

targetRep :: Target -> Depth
targetRep (Target_RGBA {})		= RGBADepth
targetRep Target_RGB  			= RGB24Depth
targetRep (Target_Bool {}) 		= RGB24Depth
targetRep (Target_Maybe_RGB)		= RGBADepth
targetRep (Target_Maybe_UI)		= RGBADepth

-- TODO: rename as targetToType 
targetType :: Target -> ExprType
targetType (Target_RGBA {})		= RGBA_Ty
targetType Target_RGB  			= RGB_Ty 
targetType (Target_Bool {}) 		= BOOL_Ty
targetType (Target_Maybe_RGB)		= Maybe_Ty RGB_Ty
targetType (Target_Maybe_UI)		= Maybe_Ty UI_Ty
targetType (Target_UI)			= UI_Ty
targetType other			= error $ show ("targetType",other)


targetToBlender :: Target -> Blender
targetToBlender Target_UI 		= Max
targetToBlender (Target_RGBA b) 	= b
targetToBlender (Target_RGB)    	= Copy
targetToBlender (Target_Maybe_UI) 	= Blend
targetToBlender (Target_Maybe_RGB) 	= Blend
targetToBlender (Target_Bool c) 	= Copy		-- ??? or Blend???

targetToBlender other 	  = error $ show ("targetToBlender",other)

-- not used yet!
targetFromType :: ExprType -> Target
targetFromType RGBA_Ty 			= Target_RGBA Copy	-- only because this is the only case
								-- used, aka buffer fmap.
targetFromType RGB_Ty 			= Target_RGB
targetFromType UI_Ty 			= Target_UI
targetFromType (Maybe_Ty UI_Ty)		= Target_Maybe_UI
targetFromType other			= error $ show ("targetFromType",other)

compileBoard2 
	:: BoardContext
	-> Target			-- merge with board context
	-> Board a
	-> IO [CBIR.Inst Int]

-- compileBoard _ _ brd | trace (show ("compileBoard",brd)) False = undefined
compileBoard2 bc t (Board ty (Trans mv brd)) 		= compileBoard2 (updateTrans mv bc) t brd
compileBoard2 bc t (Board ty (Fmap g (Board _ (Fmap h brd)))) 	
							= compileBoard2 bc t (Board ty (Fmap (g . h) brd))
compileBoard2 bc t (Board ty (Fmap g (Board ty' (Trans mv brd)))) 	
							= compileBoard2 bc t (Board ty (Trans mv (Board ty (Fmap g brd))))
compileBoard2 bc t (Board ty (Over fn above below)) 	= compileBoardOver bc t above below (targetOverBlend t (bcBlend bc))
compileBoard2 bc t (Board ty (Polygon nodes)) 		= compileBoardPolygon bc t nodes
compileBoard2 bc t (Board ty (PrimConst o)) 		= do
--	trace (show ("const",runO0 o)) $ 
	compileBoardConst bc t (evalE $ runO0 o)
compileBoard2 bc t (Board ty (Fmap f other))
--	| trace (show (bc,t,(runO1 f (E $ Var [])),ty)) False = undefined
		-- special optimization: fmap (...) (xxx :: Board Bool) can often be optimized
{- TODO
	|  goodToFmapBool argTy t tr fa
			= compileBoardFmapBool bc t 
				tr
				fa
				other (argTypeForOFun f ty) ty
-}
--	| argTy == [([],BOOL_Ty)] && trace ("fmap reject " ++ show (argTy,t,tr,fa)) False = undefined
	| otherwise = compileBoardFmap bc t (runO1 f (E brdTy $ Var [])) other argTy tyX
	where tyX    = targetType t
	      brdTy = typeOfBoard other
	      argTy = ff brdTy
	      ff (Pair_Ty t1 t2) = 
			[ (GoLeft  : p,t)  | (p,t) <- ff t1 ] ++
			[ (GoRight : p,t)  | (p,t) <- ff t2 ] 
	      ff other = [([],other)]
			
	      tr = evalE $ runO1 f (E BOOL_Ty $ O_Bool True)
	      fa = evalE $ runO1 f (E BOOL_Ty $ O_Bool False)
		
		
compileBoard2 bc t (Board ty (BufferOnBoard buffer brd)) 	= compileBufferOnBoard bc t buffer brd
	where ty = targetType t
compileBoard2 bc t (Board ty (BoardGSI fn bargs vargs)) 	= compileBoardGSI bc t fn bargs vargs
compileBoard2 bc t@(Target_RGB) (Board ty (BoardUnAlpha back fn)) = do
	inst1 <- compileBoard2 bc t back
	newBoard <- newNumber
	inst2 <- compileBoard2 (bc { bcDest = newBoard }) (Target_RGBA Blend) fn
	return $ [ Nested ("BoardUnAlpha") $
		    [ Allocate 
		       	newBoard 
        		(bcSize bc)
       			RGBADepth          -- depth of buffer	
			(BackgroundRGBADepth (RGBA 0 0 0 0))
		    ] ++ inst1
		      ++ [ copyBoard (bcDest bc) newBoard ]
		      ++ inst2
		      ++ [ copyBoard newBoard (bcDest bc) ]
		      ++ [ Delete newBoard ]
	         ]
compileBoard2 bc t other          		= error $ show ("compileBoard2",bc,t,other)

-- Drawing something *over* something.
compileBoardOver bc t above below (b1,Nothing) = compileBoard2 (bc { bcBlend = b1})  t above
compileBoardOver bc t above below (b1,Just b2) = do
	-- not quite right; assumes that the backing board is white.
	before <- compileBoard2 (bc { bcBlend = b2}) t below
	after  <- compileBoard2 (bc { bcBlend = b1}) t above
	return   [ Nested ("over: " ++ show t)
		   ( before ++
		     [Nested "`over`" []] ++
		     after 
		   )
		 ]

-- Drawing something onto the screen
compileBoardPolygon bc (Target_Bool (RGB r g b)) nodes = do
	return $ [ Nested ("precision factor = " ++ show res)
		     [ Splat (bcDest bc) 
		             Copy
		             (SplatColor' (RGBA r g b 1)  (map (mapPoint (bcTrans bc)) (nodes res)))
		     ]
		]
  where
	[(x0,y0),(x1,y1),(x2,y2)] = map (mapPoint [ Scale (a,b) | Scale (a,b) <- bcTrans bc]) [(0,0),(1,0),(0,1)]
	res   = 1 + max (abs (fromIntegral x * (x0 - x1))) (abs (fromIntegral y * (y0 - y2)))
	(x,y)  = bcSize bc

-- draw a constant board.
compileBoardConst bc t@(Target_Bool rgb) (Just (E _ (O_Bool True)))
		| targetRep t == targetRep Target_RGB 
			-- sanity check, then use the RGB drawing
		= compileBoardConst bc (Target_RGB) (Just (E RGB_Ty (O_RGB rgb)))
compileBoardConst bc t@(Target_Bool rgb) (Just (E _ (O_Bool False)))
	   	-- background *is* false
		= return []
compileBoardConst bc t@(Target_RGB) (Just (E _ (O_RGB (RGB r g b))))
		= return [
		 	(Splat (bcDest bc)
		 	       Copy 
		 	       (SplatColor' (RGBA r g b 1) [(0,0),(1,0),(1,1),(0,1)])
		 	)
			]
compileBoardConst bc t@(Target_UI) (Just (E _ (Lit r)))
		= return [
		 	(Splat (bcDest bc)
		 	       (bcBlend bc) 
		 	       (SplatColor' (RGBA r 0 0 1) [(0,0),(1,0),(1,1),(0,1)])
		 	)
			]
compileBoardConst bc t@(Target_RGBA Copy) (Just (E _ (O_RGBA rgba)))
		= do
		newBoard <- newNumber
		return [ Nested ("Const (a :: RGBA)") $
			  [ Allocate 
		        	newBoard 	   -- tag for this ChalkBoardBufferObject
        			(1,1)		   -- tiny board
        			RGBADepth          -- depth of buffer	
				(BackgroundRGBADepth rgba)
			  , copyBoard newBoard (bcDest bc) 
			  , Delete newBoard
			  ]
			]
compileBoardConst bc t@(Target_RGBA Blend) (Just (E _ (O_RGBA rgba)))
		= do
		newBoard <- newNumber
		return [ Nested ("Const (a :: RGBA)") $
			  [ Splat (bcDest bc)
			          Blend
			          (SplatColor' rgba [(0,0),(1,0),(1,1),(0,1)])
			  ]
			]

compileBoardConst bc t@(Target_Maybe_RGB) (Just (E  (Maybe_Ty RGB_Ty) (O_Just (E RGB_Ty (O_RGB (RGB r g b))))))
		= do
		newBoard <- newNumber
		return [ Nested ("Const (Just a :: Maybe RGB)") $
			  [ Splat (bcDest bc)
			          Copy
			          (SplatColor' (RGBA r g b 1) [(0,0),(1,0),(1,1),(0,1)])
			  ]
			]

compileBoardConst bc t constant = error $ show ("compileBoardConst",bc,t,constant)


data FmapArg where
	FmapArg :: Board a -> ExprType -> Path -> FmapArg

assignFrag RGBA_Ty expr = "  gl_FragColor.rgba = " ++ expr ++ ";\n"	-- TODO: assumes merging???
assignFrag (Maybe_Ty RGB_Ty) expr = "  gl_FragColor.rgba = " ++ expr ++ ";\n"	
assignFrag (Maybe_Ty UI_Ty) expr = "  gl_FragColor.rgba = " ++ expr ++ ";\n"
assignFrag RGB_Ty expr = "  gl_FragColor.rgb = " ++ expr ++ ";\n  gl_FragColor.a = 1.0;\n"
assignFrag BOOL_Ty expr = "  gl_FragColor.rgb = " ++ expr ++ ";\n  gl_FragColor.a = 1.0;\n"
assignFrag UI_Ty expr = "  gl_FragColor.r = " ++ expr ++ ";\n  gl_FragColor.a = 1.0;\n"
assignFrag other expr = error $ show ("assignFrag",other,expr)
--
-- Fmap works by basically handling conversions between all the supported types
-- then figuring out what the *code* is secondly.


prelude = unlines
	[ "vec4 cb_Alpha(float a,vec3 x) { return vec4(x.r,x.g,x.b,a); }"
	, "vec3 cb_UnAlpha(vec4 x) { return vec3(x.r,x.g,x.b) * x.a; }" 
	, "vec4 cb_WithMaskRGB(vec3 c,bool x) { return mix(vec4(0.0,0.0,0.0,0.0),vec4(c.r,c.g,c.b,1.0),x ? 1.0 : 0.0); }" 
	, "vec3 cb_WithDefaultRGB(vec3 c1,vec4 c2) { return mix(c1,c2.rgb,c2.a); }"
	, "vec4 cb_WithMaskUI(float c,bool x) { return mix(vec4(0.0,0.0,0.0,0.0),vec4(c,0.0,0.0,1.0),x ? 1.0 : 0.0); }" 
	, "float cb_WithDefaultUI(float c1,vec4 c2) { return mix(c1,c2.r,c2.a); }"
	, "vec4 cb_Just_RGB(vec3 c) { return vec4(c.r,c.g,c.b,1.0); }"	-- could reuse cb_WithMaskRGB
	]

-- TODO: The good thing about a boolean argument is that it can only have two possible values.
compileBoardFmap :: BoardContext -> Target -> E -> Board a -> [([Path],ExprType)] -> ExprType -> IO [Inst Int]
compileBoardFmap bc t (E _ty f) other argTypes resTy = do
	(insts,idMap) <- compileFmapArgs bc other argTypes
	let env = Map.fromList [ (path,"cb_sampler" ++ show n) | ((_,path),n) <- zip idMap [0..]]
	let expr = compileFmapFun env f resTy	
 	let (x0,x1,y0,y1) = (0,1,0,1) 
	let fn = 
		unlines [ "uniform sampler2D cb_sampler" ++ show n ++ ";" | (_,n) <- zip idMap [0..]] ++
		prelude ++
		"void main(void) {\n" ++
		assignFrag resTy expr ++
		"}\n"
{-
	fn' <- if length fn == 1688 then do
		   print "replacing ..."
		   fn <- readFile "XX" 
		   print fn
		   return fn
		 else return $ fn
	let fn = fn'
-}
--	putStrLn "----------"
--	putStrLn fn
--	putStrLn "----------"
	newFrag <- newNumber
	return $ insts ++
		 [ AllocFragmentShader newFrag fn []
		 , Splat (bcDest bc)
		         (bcBlend bc)
		         (SplatFunction' newFrag 
				[ ("cb_sampler" ++ show n,bid) | ((bid,_),n) <- zip idMap [0..]]
				[]
				[ (x,y) | (x,y) <- [(x0,y0),(x1,y0),(x1,y1),(x0,y1)]])
		 , Delete newFrag	-- really should cache these
		 ] ++
		 [ Delete bId | bId <- List.nub (map fst idMap) ]


{-
		++ create_Boards
		++ concat fill_Boards_RGB
		++ concat fill_Boards_Bool
		++ concat fill_Boards_UI
		++ [ SplatWithFunction newFrag 
				[ (nm,brdId) 
				| (nm,brdId) <- zip (map Prelude.fst boards_RGB ++ map Prelude.fst boards_Bool  ++ map Prelude.fst boards_UI)
						    (num_for_boards_RGB ++ num_for_boards_Bool ++ num_for_boards_UI)
				]
				vargs
				(bcDest bc) 
				[PointMap (x,y) (x,y) | (x,y) <- [(x0,y0),(x1,y0),(x1,y1),(x0,y1)]]
		   ]
		++ delete_Boards
-}

	

-- Abort!
compileBoardFmap bc t f other argTy resTy = error $ show ("compileBoardFmap",bc,t,other,argTy,resTy)


-- TODO: Add Target_Maybe_RGB here
-- use this to check if you can fmap over the Bool
goodToFmapBool [([],BOOL_Ty)] (Target_RGBA Blend) (Just (E _ (O_RGBA (RGBA _ _ _ 1)))) (Just (E _ (O_RGBA (RGBA _ _ _ 0)))) = True -- to: RM
goodToFmapBool [([],BOOL_Ty)] (Target_RGB) (Just (E _ (O_RGB {}))) (Just (E _ (O_RGB (RGB {})))) = True
goodToFmapBool _ _ _ _ = False

compileBoardFmapBool bc t@(Target_RGBA Blend) 
		(Just (E _ (O_RGBA (RGBA r g b 1))))
		(Just (E _ (O_RGBA (RGBA _ _ _ 0))))		-- the background *better* be transparent.
		other argTypes resTy = do
	compileBoard2 bc (Target_Bool (RGB r g b)) other
compileBoardFmapBool bc t@(Target_RGB) 
		(Just (E _ (O_RGB tCol)))
		(Just (E _ (O_RGB fCol@(RGB r g b))))
		other argTypes resTy = do
			insts <- compileBoard2 bc (Target_Bool tCol) other
			return $ [ Splat (bcDest bc)
			                 Copy 
			                 (SplatColor' (RGBA r g b 1) [(0,0),(1,0),(1,1),(0,1)])
				 ] ++ insts
compileBoardFmapBool bc t tr fa other argTypes resTy = do
	error $ "Found fmap over (Board Bool) (perhaps non transparent background?) " ++ show (t,tr,fa)

	

-- It is the responsability of the caller to unallocate the returned bufferids.
compileFmapArgs :: BoardContext -> Board a -> [([Path],ExprType)] -> IO ([Inst Int],[(BufferId,[Path])])
--compileFmapArgs bc brd tyMap | trace (show ("compileFmapArgs",bc,brd,tyMap)) False = undefined
compileFmapArgs bc (Board _ (Zip b1 b2)) ty | not (null ty) = do
	(insts1,mp1) <- compileFmapArgs bc b1 [ (p,t) | (Expr.GoLeft:p,t) <- ty ]
	(insts2,mp2) <- compileFmapArgs bc b2 [ (p,t) | (Expr.GoRight:p,t) <- ty ]
	return $ (insts1 ++ insts2, 
		  [ (bid,Expr.GoLeft:p) | (bid,p) <- mp1 ] ++
		  [ (bid,Expr.GoRight:p) | (bid,p) <- mp2 ]
		 )
compileFmapArgs bc (Board _ (Zip {})) ty = error $ "found zip of boards, without zip types" ++ show ty
compileFmapArgs bc brd [([],ty)] = do
	-- Otherwise, we allocate a board, construct it, and pass it back.
	(rest,bid) <- allocAndCompileBoard bc ty brd
	return (rest,[(bid,[])])
compileFmapArgs bc brd tyMap = error $ show ("compileFmapArgs",bc,brd,tyMap)

allocAndCompileBoard
	:: BoardContext		-- ignore the bcDest
	-> ExprType		-- should this be the target type????
	-> Board a
	-> IO ([CBIR.Inst Int],BufferId)
allocAndCompileBoard bc RGB_Ty brd = do
	newBoard <- newNumber
	rest <- compileBoard2 (bc { bcDest = newBoard }) Target_RGB brd
	return ( [ Nested ("alloc RGB_Ty") $
			  [ Allocate 
		        	newBoard 	   -- tag for this ChalkBoardBufferObject
        			(bcSize bc)		   -- tiny board
        			RGB24Depth          -- depth of buffer	
				(BackgroundRGB24Depth (RGB 1 1 1))
			  ] ++ rest
		], newBoard )
-- TODO: we need to somehome pass in the backing color here.

allocAndCompileBoard bc RGBA_Ty brd = do
	newBoard <- newNumber
	rest <- compileBoard2 (bc { bcDest = newBoard }) (Target_RGBA Copy) brd	-- TODO: could this be Blend???
	return ( [ Nested ("alloc RGBA_Ty") $
			  [ Allocate 
		        	newBoard 	   -- tag for this ChalkBoardBufferObject
        			(bcSize bc)		   -- tiny board
        			RGBADepth          -- depth of buffer	
				(BackgroundRGBADepth (RGBA 0 0 0 1))
			  ] ++ rest
		], newBoard )
allocAndCompileBoard bc (Maybe_Ty UI_Ty) brd = do
	newBoard <- newNumber
	rest <- compileBoard2 (bc { bcDest = newBoard }) (Target_Maybe_UI) brd	-- TODO: could this be Blend???
	return ( [ Nested ("alloc (Maybe_Ty RGB_Ty)") $
			  [ Allocate 
		        	newBoard 	   -- tag for this ChalkBoardBufferObject
        			(bcSize bc)		   -- tiny board
        			RGBADepth          -- depth of buffer	
				(BackgroundRGBADepth (RGBA 0 0 0 0))
			  ] ++ rest
		], newBoard )
allocAndCompileBoard bc (Maybe_Ty RGB_Ty) brd = do
	newBoard <- newNumber
	rest <- compileBoard2 (bc { bcDest = newBoard }) (Target_Maybe_RGB) brd
	return ( [ Nested ("alloc RGBA_Ty") $
			  [ Allocate 
		        	newBoard 	   -- tag for this ChalkBoardBufferObject
        			(bcSize bc)		   -- tiny board
        			RGBADepth          -- depth of buffer	
				(BackgroundRGBADepth (RGBA 0 0 0 0)) -- default Nothing
			  ] ++ rest
		], newBoard )
allocAndCompileBoard bc BOOL_Ty brd = do
	newBoard <- newNumber
	rest <- compileBoard2 (bc { bcDest = newBoard }) (Target_Bool (RGB 1.0 1.0 1.0)) brd
	return ( [ Nested ("alloc BOOL_Ty") $
			  [ Allocate 
		        	newBoard 	   -- tag for this ChalkBoardBufferObject
        			(bcSize bc)		   -- tiny board
        			RGB24Depth          -- depth of buffer	
				(BackgroundRGB24Depth (RGB 0 0 0))
			  ] ++ rest
		], newBoard )
allocAndCompileBoard bc UI_Ty brd = do
	newBoard <- newNumber
	rest <- compileBoard2 (bc { bcDest = newBoard }) (Target_UI) brd
	return ( [ Nested ("alloc UI_Ty") $
			  [ Allocate 
		        	newBoard 	   -- tag for this ChalkBoardBufferObject
        			(bcSize bc)		   -- tiny board
        			RGB24Depth          -- depth of buffer	
				(BackgroundRGB24Depth (RGB 0 0 0))
			  ] ++ rest
		], newBoard )
allocAndCompileBoard bc ty brd = error $ show ("allocAndCompileBoard",bc,ty,brd)

-- what a hack! Compiles our Expr language into GLSL.
compileFmapFun :: Map [Path] String -> Expr E -> ExprType -> String
compileFmapFun env (Choose e1 e2 e3) ty =
	      "\nmix(" ++ compileFmapFunE env e2 ty ++ "," ++
			compileFmapFunE env e1 ty ++ "," ++
   	         "(" ++ compileFmapFunE env e3 BOOL_Ty ++ ") ? 1.0 : 0.0)\n";
compileFmapFun env (Mix e1 e2 e3) ty =
	      "\nmix(" ++ compileFmapFunE env e1 ty ++ "," ++
			compileFmapFunE env e2 ty ++ "," ++
		        compileFmapFunE env e3 UI_Ty ++ ")\n";
compileFmapFun env v@(Var path) ty =
	case Map.lookup path env of
	  Just varName -> coerce ("texture2D(" ++ varName ++ ",gl_TexCoord[0].st)")
	  Nothing -> error $ "Can not find Var " ++ show v
  where coerce txt = case ty of
		       BOOL_Ty -> "(" ++ txt ++ ").r > 0.5"
		       UI_Ty   -> "(" ++ txt ++ ").r"
		       RGB_Ty  ->  "(" ++ txt ++ ").rgb"
--		       RGBA_Ty -> error "can not directly access RGBA"
		       other   -> txt
compileFmapFun env (O_Bool True) BOOL_Ty = "true";
compileFmapFun env (O_Bool False) BOOL_Ty = "false";
compileFmapFun env (O_RGB (RGB r g b)) RGB_Ty =
		"vec3(" ++ show r ++ "," ++
			   show g ++ "," ++
			   show b ++ ")" 			   
compileFmapFun env (Alpha v e) RGBA_Ty =
	"cb_Alpha(" ++ show v ++ "," ++ compileFmapFunE env e RGB_Ty ++ ")"
compileFmapFun env (WithMask e1 e2) (Maybe_Ty RGB_Ty) =
	"cb_WithMaskRGB(" ++ compileFmapFunE env e1 RGB_Ty  ++ "," ++ compileFmapFunE env e2 BOOL_Ty ++ ")"
compileFmapFun env (WithMask e1 e2) (Maybe_Ty UI_Ty) =
	"cb_WithMaskUI(" ++ compileFmapFunE env e1 UI_Ty  ++ "," ++ compileFmapFunE env e2 BOOL_Ty ++ ")"
compileFmapFun env (WithDefault e1 e2) RGB_Ty =
	"cb_WithDefaultRGB(" ++ compileFmapFunE env e1 RGB_Ty  ++ "," ++ compileFmapFunE env e2 (Maybe_Ty RGB_Ty) ++ ")"
compileFmapFun env (WithDefault e1 e2) UI_Ty =
	"cb_WithDefaultUI(" ++ compileFmapFunE env e1 UI_Ty  ++ "," ++ compileFmapFunE env e2 (Maybe_Ty UI_Ty) ++ ")"
compileFmapFun env (Lit v) UI_Ty = show v

compileFmapFun env (NOT e) BOOL_Ty =
		"!(" ++ compileFmapFunE env e BOOL_Ty ++ ")"
compileFmapFun env (EQUAL e1 e2) BOOL_Ty = 
		"(" ++ compileFmapFunE env e1 (error "XX") ++ 
	   ") == (" ++ compileFmapFunE env e2 (error "XX") ++ 
           ")"
compileFmapFun env (IsJust e1@(E (Maybe_Ty UI_Ty) _)) BOOL_Ty = 
		"(" ++ compileFmapFunE' env e1 ++ ").a > 0.5" 
compileFmapFun env (IsJust e1@(E (Maybe_Ty RGB_Ty) _)) BOOL_Ty =
		"(" ++ compileFmapFunE' env e1 ++ ").a > 0.5" 
compileFmapFun env (UnJust e1) UI_Ty =
		"(" ++ compileFmapFunE' env e1 ++ ").r" 
compileFmapFun env (UnJust e1) RGB_Ty =
		"(" ++ compileFmapFunE' env e1 ++ ").rgb" 
compileFmapFun env (O_Just e1) (Maybe_Ty UI_Ty) =
		"vec4(" ++ compileFmapFunE' env e1 ++ ",0.0,0.0,1.0)"
compileFmapFun env (O_Just e1) (Maybe_Ty RGB_Ty) =
		"cb_Just_RGB(" ++ compileFmapFunE' env e1 ++ ")"
compileFmapFun env (O_Nothing) (Maybe_Ty UI_Ty) = "vec4(0.0,0.0,0.0,0.0)"
compileFmapFun env (O_Nothing) (Maybe_Ty RGB_Ty) = "vec4(0.0,0.0,0.0,0.0)"

compileFmapFun env e ty = error $ show ("compileFmapFun",env,e,ty)


-- Ha! Can use internal ty.
compileFmapFunE env (E ty' e) ty = compileFmapFun env e ty'
compileFmapFunE' env (E ty' e) = compileFmapFun env e ty'

compileBufferOnBoard bc t (Buffer _ low@(x0,y0) high@(x1,y1) buffer) brd = do
	insts1          <- compileBoard2 bc t brd
	(insts2,buffId) <- compileBuffer2 t low high buffer
	
	let (x,y) = bcSize bc
	-- TODO!
	-- really, this is about 0 and 1, not x and y.
	let mv0 = Scale (fromIntegral (1 + x1-x0) / fromIntegral 1,
			fromIntegral (1 + y1-y0) / fromIntegral 1)
	let mv1 = Move (fromIntegral x0,fromIntegral y0)			
	let tr = bcTrans (updateTrans mv0 $ updateTrans mv1 bc)
	
	return $ 
		[ Nested "buffer inside board (...)" $
			insts1 ++ insts2 ++ 
			[ Splat (bcDest bc)
			        (bcBlend bc)
			        (SplatPolygon' buffId -- need a version that does no merging, but just copies
		    		        [ PointMap (x,y) (mapPoint tr (x,y))
		    		        | (x,y) <- [(0,0),(1,0),(1,1),(0,1)]
		    		        ] )
			, Delete buffId
			]
	       ]


compileBuffer2 
	:: Target
	-> (Int,Int)
	-> (Int,Int)
	-> InsideBuffer a
	-> IO ([CBIR.Inst Int],BufferId)

-- TODO: This should be a Copy or a Blend???
compileBuffer2 (Target_RGBA Copy) low high (ImageRGBA bs) = do
	compileByteStringImage low high bs RGBADepth
-- TODO: Not sure about this. what does blend mean in this context,
-- because this *always* allocs a new board.
-- Perhaps we need a mark function, that  maps Blend to Copy, for compileBufferOnBoard?
compileBuffer2 (Target_RGBA Blend) low high (ImageRGBA bs) = do
	compileByteStringImage low high bs RGBADepth
compileBuffer2 Target_RGB low high (ImageRGB bs) = do
	compileByteStringImage low high bs RGB24Depth
compileBuffer2 Target_UI low high (ImageUI bs) = do
	compileByteStringImage low high bs' RGB24Depth
  where bs' = BS.concatMap (\ w -> BS.pack [w,0,0]) bs

-- TODO: common up with other fmap function. Not that Buffer can *not* use zip.
compileBuffer2 t low@(x0,y0) high@(x1,y1) (FmapBuffer f buff argTy) = do
	let tarTy = targetType t
	let expr  = runO1 f (E argTy $ Var [])
				-- only place targetFromType is used!!!
	(insts,bId) <- compileBuffer2 (targetFromType argTy) low high buff
	let env = Map.fromList [ ([],"cb_sampler0") ]
	let code = compileFmapFunE env expr tarTy	
	let fn = 
		unlines [ "uniform sampler2D cb_sampler0;" ] ++
		prelude ++
		"void main(void) {\n" ++
		assignFrag tarTy code ++
		"}\n"

--	putStrLn fn
	newFrag <- newNumber
	targetBuff <- newNumber
	return ( insts ++
		 [ allocateBuffer (1+x1-x0,1+y1-y0) targetBuff t
		 , AllocFragmentShader newFrag fn []
		 , Splat targetBuff
	         	Copy		-- copy, because it is a new board
		         (SplatFunction' newFrag 
				[ ("cb_sampler0",bId)]
				[]
				[ (x,y) | (x,y) <- let (x0,x1,y0,y1) = (0,1,0,1) 
								 in [(x0,y0),(x1,y0),(x1,y1),(x0,y1)]]
                         )
		 , Delete newFrag	-- really should cache these
		 , Delete bId 
		 ], targetBuff )



	
compileBuffer2 t low@(x0,y0) high@(x1,y1) (BoardInBuffer brd) = do
	newBoard <- newNumber	
	let (sx,sy) = (1 + x1 - x0, 1 + y1 - y0)
	let mv = Move (fromIntegral x0,fromIntegral y0)
	let sc = Scale (1 / fromIntegral sx,1 / fromIntegral sy)
	let bc = updateTrans mv $ updateTrans sc $ initBoardContext (sx,sy) 0
	allocAndCompileBoard bc (targetType t) brd
{-
	-- we want to map (x0,y0) x (x1,y1) onto the Board (0,0) x (1,1)
	rest <- compileBoard compileBoardRGBA bc brd
	return ( [ Nested ("BoardInBuffer") $
		   [ Allocate 
			newBoard 	-- tag for this ChalkBoardBufferObject
			(sx,sy)	   	-- we know size
			RGBADepth       -- depth of buffer
			(BackgroundRGBADepth (RGBA 1 1 1 1))
		   ] ++ rest
	         ], newBoard)
-}

{-	-- assume def is transparent!
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
-}	
	
compileBuffer2 t low high buffer = error $ show ("compileBuffer2",t,(low,high),buffer)
	
-- Allocate a buffer; we do not care about color
allocateBuffer (sx,sy) newBoard Target_RGB =
		Allocate 
			newBoard 	-- tag for this ChalkBoardBufferObject
			(sx,sy)	   	-- we know size
			RGB24Depth       -- depth of buffer
			(BackgroundRGB24Depth (RGB 0 0 0))
allocateBuffer (sx,sy) newBoard (Target_RGBA Copy) =
		Allocate 
			newBoard 	-- tag for this ChalkBoardBufferObject
			(sx,sy)	   	-- we know size
			RGBADepth       -- depth of buffer
			(BackgroundRGBADepth (RGBA 0 0 0 0))
allocateBuffer (sx,sy) newBoard Target_Maybe_UI =
		Allocate 
			newBoard 	-- tag for this ChalkBoardBufferObject
			(sx,sy)	   	-- we know size
			RGBADepth       -- depth of buffer
			(BackgroundRGBADepth (RGBA 0 0 0 0))
allocateBuffer (sx,sy) newBoard t = error $ show ("allocateBuffer",(sx,sy),newBoard,t)


-- TODO: use allocAnd... to build this.
compileBoardGSI 
	:: BoardContext 
	-> Target 
	-> String 
	-> [(String,UniformTexture)] 
	-> [(String,UniformArgument)] 
	-> IO [Inst Int]
compileBoardGSI bc Target_RGB fn bargs vargs = do
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
	 	<- sequence [ compileBoard2  (bc { bcDest = brdId }) Target_RGB brd
			    | ((_,brd),brdId) <- zip boards_RGB num_for_boards_RGB
			    ]
	fill_Boards_Bool
	 	<- sequence [ compileBoard2 (bc { bcDest = brdId }) (Target_Bool (RGB 1 1 1)) brd
			    | ((_,brd),brdId) <- zip boards_Bool num_for_boards_Bool
			    ]
	fill_Boards_UI
	 	<- sequence [ compileBoard2 (bc { bcDest = brdId }) Target_UI brd
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
		++ [ Splat (bcDest bc)
		         (bcBlend bc)
		           (SplatFunction' newFrag 
				[ (nm,brdId) 
				| (nm,brdId) <- zip (map Prelude.fst boards_RGB ++ map Prelude.fst boards_Bool  ++ map Prelude.fst boards_UI)
						    (num_for_boards_RGB ++ num_for_boards_Bool ++ num_for_boards_UI)
				]
				vargs
				[(x,y) | (x,y) <- [(x0,y0),(x1,y0),(x1,y1),(x0,y1)]]
			   )
		   ]
		++ delete_Boards




{-

 Challenge



   compile    Board (RGBA -> RGBA)

 it requires

    RGBAtoRGBA

and 

    RGB <- seed.

  compile :: context -> fmap (f (...) (...)) (Board XX)



-}

