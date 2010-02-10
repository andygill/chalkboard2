{-# LANGUAGE TypeFamilies, ExistentialQuantification, GADTs, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Graphics.ChalkBoard.Internals
	( Board(..)
	, InsideBoard(..)
	, Buffer(..)
	, InsideBuffer(..)
	, Trans(..)
	, UniformArgument(..)
	, UniformTexture(..)
	, Argument(..)
	, TextureSize(..)
	, board
	, buffer
	, uniform
	, typeOfBoard
	, typeOfBuffer
	) where
		
import Graphics.ChalkBoard.Types
--import Graphics.ChalkBoard.O
import Graphics.ChalkBoard.O.Internals
import Graphics.ChalkBoard.Expr
import Graphics.ChalkBoard.IStorable as IS
import Data.ByteString
--import Graphics.Rendering.OpenGL.GL.Shaders (Uniform)

data Buffer a = Buffer ExprType (Int,Int) (Int,Int) (InsideBuffer a)

-- TODO: an intermeduate between Buffer and InsideBuffer, that caches the type.
-- This avoids the hack inside FmapBuffer.

data InsideBuffer a where
	BoardInBuffer	:: Board a -> InsideBuffer a
			-- ExprType is the type of the inner inside Buffer
	FmapBuffer	:: forall b . (O b -> O a) -> InsideBuffer b -> ExprType -> InsideBuffer a
		-- we represent image as mutable arrays simply because
		-- we need a quick way to get to a pointer to the array
		-- They are really actually constant.
	ImageG		:: ByteString -> InsideBuffer UI
	ImageRGB	:: ByteString -> InsideBuffer RGB
	ImageRGBA	:: ByteString -> InsideBuffer (RGBA -> RGBA)	-- later, Maybe RGB
	ImageUI		:: ByteString -> InsideBuffer UI
	FlipLR		:: InsideBuffer a -> InsideBuffer a
	FlipTB		:: InsideBuffer a -> InsideBuffer a
	
data Board a = Board ExprType (InsideBoard a)

data InsideBoard a where
	PrimConst 	:: (O a)					-> InsideBoard a
	Trans 		:: Trans -> Board a				-> InsideBoard a
	Crop 		:: ((R,R),(R,R)) -> Board a			-> InsideBoard (Maybe a)
	Fmap :: forall b . (O b -> O a) -> Board b			-> InsideBoard a
	Zip		:: Board b -> Board c 				-> InsideBoard (b,c)
	Polygon 	:: (Float -> [(R,R)]) 				-> InsideBoard Bool	-- later, have two types of Polygon
	-- only used in code generator, when types do not matter.
	Over  		:: 	(a -> a -> a) -> Board a -> Board a 	-> InsideBoard a
	BufferOnBoard	:: Buffer a -> Board a 				-> InsideBoard a
	-- FFI into the Graphics shader langauge.
	BoardGSI 	:: String -> [(String,TextureSize,UniformTexture)] -> [(String,UniformArgument)] -> InsideBoard a	
	BoardUnAlpha	:: Board RGB -> Board (RGBA -> RGBA) 		-> InsideBoard RGB

data UniformArgument = UniformArgument Argument
		     | UniformListArgument [Argument]
	deriving Show

data UniformTexture  = BoardRGBArgument (Board RGB)
		     | BoardBoolArgument (Board Bool)
		     | BoardUIArgument (Board UI)
		     | BoardMaybeRGBArgument (Board (Maybe RGB))
		     | BoardRGBAFnArgument (Board (RGBA -> RGBA))
		     | BufferRGBAFnArgument (Buffer (RGBA -> RGBA))
	deriving Show

data TextureSize = ResultSize
		 | ActualSize Int Int

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
  buffer :: Buffer a -> UniformTexture

instance UniformBoard RGB where
  board = BoardRGBArgument

instance UniformBoard Bool where
  board = BoardBoolArgument

instance UniformBoard Float where
  board = BoardUIArgument

instance UniformBoard (Maybe RGB) where
  board = BoardMaybeRGBArgument

instance UniformBoard (RGBA -> RGBA) where
  board = BoardRGBAFnArgument
  buffer = BufferRGBAFnArgument

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
	show (Board ty b) = "(" ++ show b ++ " :: " ++ show ty ++ ")"

instance Show (InsideBoard a) where
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
	show (BoardGSI _ arg1 arg2) = "BoardGSI ffi " 
				++ show (Prelude.map (\ (x,_,_) -> x) arg1) 
				++ show (Prelude.map Prelude.fst arg2) 

instance Show (Buffer a) where
	show (Buffer ty x y a) = "(Buffer " ++ show (x,y) ++ " " ++ show a ++ " " ++ show ty ++ ")"

instance Show (InsideBuffer a) where
	show (BoardInBuffer brd) = "BoardInBuffer (" ++ show brd ++ ")"
	show (FmapBuffer _ _ brd) = "FmapBuffer (..) (" ++ show brd ++ ")"
	show (ImageRGB _)        = "ImageRGB (..)"
	show (ImageRGBA _)        = "ImageRGBA (..)"
	show (ImageUI _)        = "ImageUI (..)"
	
data Trans = Move (R,R)
	   | Scale (R,R)
	   | Rotate Radian
	deriving Show

-- TODO: add ty to Board (and Buffer), to make this cheap
typeOfBoard :: Board a -> ExprType	
typeOfBoard (Board ty _)   = ty

typeOfBuffer :: Buffer a -> ExprType
typeOfBuffer (Buffer ty _ _ _) = ty
