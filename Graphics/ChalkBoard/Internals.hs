{-# LANGUAGE TypeFamilies, ExistentialQuantification, GADTs, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Graphics.ChalkBoard.Internals
	( Board(..)
	, Buffer(..)
	, InsideBuffer(..)
	, Trans(..)
	, UniformArgument(..)
	, UniformTexture(..)
	, Argument(..)
	, board
	, uniform
	, boardType
	) where
		
import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.O
import Graphics.ChalkBoard.O.Internals
import Graphics.ChalkBoard.Expr
import Graphics.ChalkBoard.IStorable as IS
import Data.ByteString
import Graphics.Rendering.OpenGL.GL.Shaders (Uniform)

data Buffer a = Buffer (Int,Int) (Int,Int) (InsideBuffer a)

data InsideBuffer a where
	BoardInBuffer	:: Board a -> InsideBuffer a
	FmapBuffer	:: forall b . (O b -> O a) -> InsideBuffer b -> InsideBuffer a
		-- we represent image as mutable arrays simply because
		-- we need a quick way to get to a pointer to the array
		-- They are really actually constant.
	Image		:: ReadOnlyCByteArray -> InsideBuffer a	-- RGB or RGBA
	ImageG		:: ByteString -> InsideBuffer UI
	ImageRGB	:: ByteString -> InsideBuffer RGB
	ImageRGBA	:: ByteString -> InsideBuffer (RGBA -> RGBA)	-- later, Maybe RGB
	ImageUI		:: ByteString -> InsideBuffer UI
	FlipLR		:: InsideBuffer a -> InsideBuffer a
	FlipTB		:: InsideBuffer a -> InsideBuffer a
	
data Board a where
	PrimConst 	:: (O a)					-> Board a
	Trans 		:: Trans -> Board a				-> Board a
	Crop 		:: ((R,R),(R,R)) -> Board a			-> Board (Maybe a)
	Fmap :: forall b . (O b -> O a) -> Board b			-> Board a
	Zip		:: Board b -> Board c 				-> Board (b,c)
	Polygon 	:: (Float -> [(R,R)]) 				-> Board Bool	-- later, have two types of Polygon
	-- only used in code generator, when types do not matter.
	Over  		:: 	(a -> a -> a) -> Board a -> Board a 	-> Board a
	BufferOnBoard	:: Buffer a -> Board a 				-> Board a
	-- FFI into the Graphics shader langauge.
	BoardGSI 	:: String -> [(String,UniformTexture)] -> [(String,UniformArgument)] -> Board a	
	BoardUnAlpha	:: Board RGB -> Board (RGBA -> RGBA) 		-> Board RGB

data UniformArgument = UniformArgument Argument
		     | UniformListArgument [Argument]
	deriving Show

data UniformTexture  = BoardRGBArgument (Board RGB)
		     | BoardBoolArgument (Board Bool)
		     | BoardUIArgument (Board UI)
		
	deriving Show

--		     | forall a . (GSArg a) => ScalarArg a
--		     | forall a . (GSArg a) => VectorArg a
---		     | forall a . (GSArg a) => VectorOfVectorArg a

data Argument
	= Int Int
	| Float Float
	| Vec2 (Float,Float)
	| Arr [Float]
	| ArrVec2 [(Float,Float)]
	| Vec3 (Float,Float,Float)
	| Vec4 (Float,Float,Float,Float)
	deriving Show

class UniformBoard a where 
  board :: Board a -> UniformTexture

instance UniformBoard RGB where
  board = BoardRGBArgument

instance UniformBoard Bool where
  board = BoardBoolArgument

instance UniformBoard Float where
  board = BoardUIArgument


uniform  :: Argument -> UniformArgument
uniform = UniformArgument

{-
   want to allow

    :: Float
    :: Int
    :: (Float,Float)
    :: (Float,Float,Float)
    :: (Float,Float,Float,Float)
    :: [any of the above]

-}

instance Show (Board a) where
--	show (PrimFun {}) = "PrimFun"
	show (PrimConst {}) = "PrimConst"
	show (Trans _ brd)  = "Trans (..) (" ++ show brd ++ ")"
	show (Polygon {})   = "Polygon"
--	show (Circle {})    = "Circle"
	show (Fmap _ brd)   = "Fmap (..) (" ++ show brd ++ ")"
	show (Zip brd1 brd2)   = "Zip (" ++ show brd1 ++ ") (" ++ show brd2 ++ ")"
	show (Over _ brd1 brd2)   = "Over (..) (" ++ show brd1 ++ " " ++ show brd2 ++ ")"
	show (BufferOnBoard buff brd)  = "BufferOnBoard (" ++ show buff ++ ") (" ++ show brd ++ ")"
	show (BoardUnAlpha b1 b2)  = "BoardUnAlpha (" ++ show b1 ++ ") (" ++ show b2 ++ ")"
	show (BoardGSI nm arg1 arg2) = "BoardGSI ffi " 
				++ show (Prelude.map Prelude.fst arg1) 
				++ show (Prelude.map Prelude.fst arg2) 

instance Show (Buffer a) where
	show (Buffer x y a) = "Buffer " ++ show (x,y) ++ " " ++ show a

instance Show (InsideBuffer a) where
	show (BoardInBuffer brd) = "BoardInBuffer (" ++ show brd ++ ")"
	show (FmapBuffer _ brd) = "FmapBuffer (..) (" ++ show brd ++ ")"
	show (Image arr)        = "Image (..)"
	show (ImageRGB arr)        = "ImageRGB (..)"
	show (ImageRGBA arr)        = "ImageRGBA (..)"
	show (ImageUI arr)        = "ImageUI (..)"
	
data Trans = Move (R,R)
	   | Scale (R,R)
	   | Rotate Radian
	deriving Show

-- TODO: add ty to Board (and Buffer), to make this cheap
boardType :: Board a -> ExprType	
boardType (PrimConst o)   = typeO o
boardType (Polygon {})    = BOOL_Ty
boardType (Trans _ brd)   = boardType brd
boardType (Over _ lhs _)  = boardType lhs
boardType (Zip a b)       = Pair_Ty (boardType a) (boardType b)
boardType (Fmap f brd)    = typeO1 f (boardType brd)
boardType other = error $ "boardType of " ++ show other