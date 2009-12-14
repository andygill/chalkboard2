{-# LANGUAGE TypeFamilies, ExistentialQuantification, GADTs, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Graphics.ChalkBoard.Board 
	( -- * The 'Board' 
	  Board
	  -- * Ways of manipulating 'Board'.
	, (<$>)
	, move
	, rotate
	, scaleXY
	  -- * Ways of creating a new 'Board'.
	, boardOf
	, circle
	, box
	, square
	, triangle
	, polygon
	, readBoard
	, bufferOnBoard
	, readNormalizedBoard
	) where


import Graphics.ChalkBoard.Internals
import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.O as O
import Graphics.ChalkBoard.O.Internals
import Graphics.ChalkBoard.Core
import Graphics.ChalkBoard.Utils
import Graphics.ChalkBoard.Expr
import Graphics.ChalkBoard.Buffer
import Graphics.ChalkBoard.IStorable as IS

import Data.Array.Unboxed  as U
import Data.Array.MArray
import Data.Array.Storable
import Data.Word
import Codec.Image.DevIL


import Prelude hiding (lookup)
-- | 'fmap' like operator over a 'Board'.

instance OFunctor Board where
  (<$>) f brd = Fmap f brd -- PrimConst (lamO $ f) <*> brd

-- | 'pure' like operator for 'Board'.	
boardOf :: O a -> Board a
boardOf = PrimConst

transPoint :: Trans -> (R,R) -> (R,R)
transPoint (Move (xd,yd)) 	(x,y) = (x - xd,y - yd)
transPoint (Scale (xn,yn)) 	(x,y) = (x / xn,y / yn)
transPoint (Rotate theta) 	(x,y) = (cos theta * x - sin theta * y,
					 sin theta * x + cos theta * y)


-- |  Generate a unit square (1 by 1 square) centered on origin
square :: Board Bool
square = Polygon (const [(-0.5,-0.5),(-0.5,0.5),(0.5,0.5),(0.5,-0.5)])

-- | Generate a unit circle (radius .5) centered on origin
circle :: Board Bool
circle = Polygon $ \ sz' -> 
	let sz = max (ceiling sz') 3
	in [ (sin x/2,cos x/2) 
	   | x <- map (* (2*pi/fromIntegral sz)) $ take sz [0..]
	   ]

-- | Generate an arbitary triangle from 3 points.
triangle :: Point -> Point -> Point -> Board Bool
triangle p1 p2 p3 = Polygon (const [p1,p2,p3])

-- | Generate a (convex) polygon from a list of points. There must be at least 3 points,
-- and the points must form a convex polygon.
polygon :: [Point] -> Board Bool
polygon = Polygon . const

-- | 'box' generate a box between two corner points)
box :: (Point,Point) -> Board Bool
box ((x0,y0),(x1,y1)) = Polygon (const [(x0,y0),(x1,y0),(x1,y1),(x0,y1)])


-- | 'move' moves the contents of 'Board'
move :: (R,R) -> Board a -> Board a
move = Trans . Move

instance Scale (Board a) where
  -- | 'scale' scales the contents of 'Board'
  scale n brd = scaleXY (n,n) brd

-- | 'scaleXY' scales the contents of 'Board' the X and Y dimension.
--  See also 'scale'.
scaleXY :: (R,R) -> Board a -> Board a
scaleXY = Trans . Scale

-- | 'rotate' rotates a 'Board' clockwise by a radian argument.
rotate :: Radian -> Board a -> Board a
rotate = Trans . Rotate

lookup :: Board a -> Float -> (R,R) -> a
lookup brd r (x,y) = unO $ lookupO brd r (x,y)

lookupO :: Board a -> Float -> (R,R) -> O a
lookupO (PrimFun f) r (x,y) = f (x,y)
lookupO (Trans t brd) r (x,y) = lookupO brd r (transPoint t (x,y))
lookupO (Fmap f brd) r (x,y) = f $ lookupO brd r (x,y)
lookupO (Polygon points) r (x,y) = 
	if insidePoly (points r) (x,y)
	then true
	else false
lookupO other r (x,y) = error $ show ("lookup",other,r,(x,y))

-- miss-use of PrimFun and primO
	
--coord :: Board (R,R)
--coord = PrimFun (\ (x,y) -> primO (O_Pair (E $ Lit x) (E $ Lit y)) $ (x,y))

instance Over a => Over (Board a) where
	-- 'over' overlays two 'Board's.
	over b1 b2 = Over over b1 b2


-- I would rather mask to be a Board Bool, and we could use <$>,
-- to choose, but the Board transformer will do for now.
mask :: ((R,R),(R,R)) -> Board a -> Board (Maybe a)
mask = Crop

-- | read a file containing a common image format (jpg, gif, etc.), and create a 'Board RGBA', and the X and Y size of the image.
readBoard :: String -> IO (Int,Int,Board RGBA)
readBoard filename = do
  buff <- readBuffer filename
  let (x,y) = bufferSize buff
  return $ (x,y,BufferOnBoard buff (boardOf (O.transparent O.white)))

{-
readFunnyBoard :: IO (Int,Int,Board RGBA)
readFunnyBoard = do
  let arr :: UArray (Int,Int,Int) Word8
      arr = array ((0,0,0),(128,255,3)) 
	$ concat [ [ ((x,y,0), if x < 16 then (if even (y `div` 16) then 255 else fromIntegral x)  else fromIntegral x)
		   , ((x,y,1), fromIntegral x)
		   , ((x,y,2), 128)
		   , ((x,y,3), 255)
		   ] 
		 | y <- [0..255]
		 , x <- [0..128]
		 ]
  iStore <- readOnlyCByteArray arr 
  let ((0,0,0), (h,w,3)) = U.bounds arr
  return $ (w+1,h+1,BufferInBoard (O.transparent O.white) (Buffer (0,0) (w,h) $ Image iStore))
-}
  
readNormalizedBoard :: String -> IO (Int,Int,Board RGBA)
readNormalizedBoard filename = do
    (x,y,imgBrd) <- readBoard (filename)
    let xy = fromIntegral $ max x y
        sc = 1 / xy
        xd = fromIntegral y / xy
        yd = fromIntegral x / xy
        img = move (-0.5 * yd,-0.5 * xd)  (scale sc imgBrd)
    return (x,y,img)


-- TODO: Consider, does this draw whole pixels, or interprelate between the center points?
bufferOnBoard :: Buffer a -> Board a -> Board a
bufferOnBoard buff brd = BufferOnBoard buff brd

