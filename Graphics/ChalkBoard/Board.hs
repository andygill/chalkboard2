{-# LANGUAGE TypeFamilies, ExistentialQuantification, GADTs, 
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, 
    UndecidableInstances #-}

module Graphics.ChalkBoard.Board 
	( -- * The 'Board' 
	  Board
	  -- * Ways of manipulating 'Board'.
	, (<$>)
	, zip
	, zipWith
	, zipWith3
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
	, unAlphaBoard
	) where


import Graphics.ChalkBoard.Internals
import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.O as O
import Graphics.ChalkBoard.O.Internals
--import Graphics.ChalkBoard.Core
--import Graphics.ChalkBoard.Utils
import Graphics.ChalkBoard.Expr
import Graphics.ChalkBoard.Buffer
--import Graphics.ChalkBoard.IStorable as IS

--import Data.Array.Unboxed  as U
--import Data.Array.MArray
--import Data.Array.Storable
--import Data.Word
--import Codec.Image.DevIL
import Prelude hiding (zip, zipWith, zipWith3,lookup)

-- | 'fmap' like operator over a 'Board'.

instance OFunctor Board where
  (<$>) f brd = Board (typeO1 f (typeOfBoard brd)) $ Fmap f brd


-- | 'pure' like operator for 'Board'.	
boardOf :: O a -> Board a
boardOf ob = Board (typeO ob) (PrimConst ob)

zip :: Board a -> Board b -> Board (a,b)
zip b1 b2 = Board (Pair_Ty (typeOfBoard b1) (typeOfBoard b2)) $ Zip b1 b2

zipWith :: (O a -> O b -> O c) -> Board a -> Board b -> Board c
zipWith f b1 b2 = (\ o' -> f (fstO o') (sndO o')) <$> (b1 `zip` b2)

zipWith3 :: (O a -> O b -> O c -> O d) -> Board a -> Board b -> Board c -> Board d
zipWith3 f b1 b2 b3 = (\ o' -> f (fstO o') (fstO (sndO o')) (sndO (sndO o'))) <$> (b1 `zip` (b2 `zip` b3))

{-
transPoint :: Trans -> (R,R) -> (R,R)
transPoint (Move (xd,yd)) 	(x,y) = (x - xd,y - yd)
transPoint (Scale (xn,yn)) 	(x,y) = (x / xn,y / yn)
transPoint (Rotate theta) 	(x,y) = (cos theta * x - sin theta * y,
					 sin theta * x + cos theta * y)
-}


-- |  Generate a unit square (1 by 1 square) centered on origin
square :: Board Bool
square = Board BOOL_Ty $ Polygon (const [(-0.5,-0.5),(-0.5,0.5),(0.5,0.5),(0.5,-0.5)])

-- | Generate a unit circle (radius .5) centered on origin
circle :: Board Bool
circle = Board BOOL_Ty $ Polygon $ \ sz' -> 
	let sz = max (ceiling sz') 3
	in [ (sin x/2,cos x/2) 
	   | x <- map (* (2*pi/fromIntegral sz)) $ take sz [0..]
	   ]

-- | Generate an arbitary triangle from 3 points.
triangle :: Point -> Point -> Point -> Board Bool
triangle p1 p2 p3 = Board BOOL_Ty $ Polygon (const [p1,p2,p3])

-- | Generate a (convex) polygon from a list of points. There must be at least 3 points,
-- and the points must form a convex polygon.
polygon :: [Point] -> Board Bool
polygon points = Board BOOL_Ty $ Polygon (const points)

-- | 'box' generate a box between two corner points)
box :: (Point,Point) -> Board Bool
box ((x0,y0),(x1,y1)) = Board BOOL_Ty $ Polygon (const [(x0,y0),(x1,y0),(x1,y1),(x0,y1)])

-- | 'move' moves the contents of 'Board'
move :: (R,R) -> Board a -> Board a
move xy brd = Board (typeOfBoard brd) $ Trans (Move xy) brd

instance Scale (Board a) where
  -- | 'scale' scales the contents of 'Board'
  scale n brd = scaleXY (n,n) brd

-- | 'scaleXY' scales the contents of 'Board' the X and Y dimension.
--  See also 'scale'.
scaleXY :: (R,R) -> Board a -> Board a
scaleXY s brd = Board (typeOfBoard brd) $ Trans (Scale s) brd

-- | 'rotate' rotates a 'Board' clockwise by a radian argument.
rotate :: Radian -> Board a -> Board a
rotate r brd = Board (typeOfBoard brd) $ Trans (Rotate r) brd

{-
lookup :: Board a -> Float -> (R,R) -> a
lookup brd r (x,y) = unO $ lookupO brd r (x,y)

lookupO :: Board a -> Float -> (R,R) -> O a
lookupO (Trans t brd) r (x,y) = lookupO brd r (transPoint t (x,y))
lookupO (Fmap f brd) r (x,y) = f $ lookupO brd r (x,y)
lookupO (Polygon points) r (x,y) = 
	if insidePoly (points r) (x,y)
	then true
	else false
lookupO other r (x,y) = error $ show ("lookup",other,r,(x,y))
-}

-- miss-use of PrimFun and primO
	
--coord :: Board (R,R)
--coord = PrimFun (\ (x,y) -> primO (O_Pair (E $ Lit x) (E $ Lit y)) $ (x,y))

instance Over a => Over (Board a) where
	-- 'over' overlays two 'Board's.
	over b1 b2 = Board (typeOfBoard b1) $ Over over b1 b2

-- I would rather mask to be a Board Bool, and we could use <$>,
-- to choose, but the Board transformer will do for now.
{-
mask :: ((R,R),(R,R)) -> Board a -> Board (Maybe a)
mask = error "mask"
-}

-- | read a file containing a common image format (jpg, gif, etc.), 
-- and create a 'Board RGBA', and the X and Y size of the image.
readBoard :: String -> IO (Int,Int,Board (RGBA -> RGBA))
readBoard filename = do
  buff <- readBuffer filename
  let (x,y) = bufferSize buff
  return $ (x,y,Board RGBA_Ty $ BufferOnBoard buff (boardOf (O.transparent)))
  
readNormalizedBoard :: String -> IO (Int,Int,Board (RGBA -> RGBA))
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
bufferOnBoard buff brd = Board (typeOfBoard brd) $ BufferOnBoard buff brd

-- call this appAlpha
unAlphaBoard :: Board RGB -> Board (RGBA -> RGBA) -> Board RGB
unAlphaBoard b1 b2 = Board RGB_Ty (BoardUnAlpha b1 b2)
