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
	) where
		
import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.O
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
	ImageRGBA	:: ByteString -> InsideBuffer RGBA
	FlipLR		:: InsideBuffer a -> InsideBuffer a
	FlipTB		:: InsideBuffer a -> InsideBuffer a
	
data Board a where
	PrimFun 	:: 		((R,R) -> O a)	-> Board a-- TODO: RM!
	PrimConst 	::		(O a)	-> Board a
	Trans 		:: Trans 	->	(Board a)	-> Board a
	Crop 		   :: ((R,R),(R,R)) -> Board a	-> Board (Maybe a)
	Fmap  	:: forall b . (O b -> O a) -> Board b	-> Board a
	Zip	:: Board b -> Board c -> Board (b,c)
	Polygon 	   ::	(Float -> [(R,R)])	-> 		Board Bool	-- later, have two types of Polygon
	-- only used in code generator, when types do not matter.
	Over  		:: 	(a -> a -> a) -> Board a -> Board a -> 	Board a
	BufferOnBoard	:: Buffer a -> Board a -> Board a
	-- FFI into the Graphics shader langauge.
	BoardGSI 	:: String -> [(String,UniformTexture)] -> [(String,UniformArgument)] -> Board a	
	
data UniformArgument = UniformArgument Argument
		     | UniformListArgument [Argument]
	deriving Show

data UniformTexture  = BoardRGBArgument (Board RGB)
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
	deriving Show

class UniformBoard a where 
  board :: Board a -> UniformTexture

instance UniformBoard RGB where
  board = BoardRGBArgument

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
	show (PrimFun {}) = "PrimFun"
	show (PrimConst {}) = "PrimConst"
	show (Trans _ brd)  = "Trans (..) (" ++ show brd ++ ")"
	show (Polygon {})   = "Polygon"
--	show (Circle {})    = "Circle"
	show (Fmap _ brd)   = "Fmap (..) (" ++ show brd ++ ")"
	show (Zip brd1 brd2)   = "Zip (" ++ show brd1 ++ ") (" ++ show brd2 ++ ")"
	show (Over _ brd1 brd2)   = "Over (..) (" ++ show brd1 ++ " " ++ show brd2 ++ ")"
	show (BufferOnBoard buff brd)  = "BufferOnBoard (" ++ show buff ++ ") (" ++ show brd ++ ")"
	show (BoardGSI nm arg1 arg2) = "BoardGSI ffi " 
				++ show (Prelude.map fst arg1) 
				++ show (Prelude.map fst arg2) 

instance Show (Buffer a) where
	show (Buffer x y a) = "Buffer " ++ show (x,y) ++ " " ++ show a

instance Show (InsideBuffer a) where
	show (BoardInBuffer brd) = "BoardInBuffer (" ++ show brd ++ ")"
	show (FmapBuffer _ brd) = "FmapBuffer (..) (" ++ show brd ++ ")"
	show (Image arr)        = "Image (..)"
	show (ImageRGB arr)        = "ImageRGB (..)"
	show (ImageRGBA arr)        = "ImageRGBA (..)"
	
data Trans = Move (R,R)
	   | Scale (R,R)
	   | Rotate Radian
	deriving Show
	