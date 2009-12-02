{-# LANGUAGE TypeFamilies, ExistentialQuantification, GADTs, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Graphics.ChalkBoard.Board.Internals
	( Board(..)
	, Trans(..)
	) where
		
import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.O
import Graphics.ChalkBoard.IStorable as IS

		
data Board a where
	PrimFun 	:: 		((R,R) -> O a)	-> Board a-- TODO: RM!
	PrimConst 	::		(O a)	-> Board a
	Trans 		:: Trans 	->	(Board a)	-> Board a
	Crop 		:: ((R,R),(R,R)) -> Board a	-> Board (Maybe a)
	Fmap  :: forall b . (O b -> O a) -> Board b	-> Board a
--	Fzip  :: forall b c . (O b -> O c -> O a) -> Board b	-> Board c -> Board a
	Polygon 	::	(Float -> [(R,R)])	-> 		Board Bool	-- later, have two types of Polygon
--	Circle		:: 						Board Bool
	-- only used in code generator, when types do not matter.
	Over  		:: 	(a -> a -> a) -> Board a -> Board a -> 	Board a
		-- we represent image as mutable arrays simply because
		-- we need a quick way to get to a pointer to the array
	Image		:: IStorableArray (Int,Int,Int) -> Board RGBA

instance Show (Board a) where
	show (PrimFun {}) = "PrimFun"
	show (PrimConst {}) = "PrimConst"
	show (Trans _ brd)  = "Trans (..) (" ++ show brd ++ ")"
	show (Polygon {})   = "Polygon"
--	show (Circle {})    = "Circle"
	show (Fmap _ brd)   = "Fmap (..) (" ++ show brd ++ ")"
	show (Image arr)    = "Image (..)"
	show (Over _ brd1 brd2)   = "Over (..) (" ++ show brd1 ++ " " ++ show brd2 ++ ")"
	
data Trans = Move (R,R)
	   | Scale (R,R)
	   | Rotate Radian
	deriving Show
	