{-# LANGUAGE TypeFamilies, ExistentialQuantification, GADTs, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Graphics.ChalkBoard.Buffer 
	( -- * The 'Board' 
	  Buffer
	  -- * Ways of manipulating 'Buffer'.
	, (<$>)
	  -- * Ways of looking at a Buffer
	, bufferBounds
	, bufferSize
	  -- * Ways of creating a new 'Board'.
	, newBufferOf
	, readBuffer
	, newBufferRGB
	, newBufferRGBA
	, newBufferUI
	, boardToBuffer
	, moveBuffer
	) where


import Graphics.ChalkBoard.Internals
import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.O
import Graphics.ChalkBoard.O.Internals
import Graphics.ChalkBoard.Core
import Graphics.ChalkBoard.Utils
import Graphics.ChalkBoard.Expr
import Graphics.ChalkBoard.IStorable as IS

import Data.Array.Unboxed  as U
import Data.Array.MArray
import Data.Array.Storable
import Data.Word
import Data.ByteString as BS

import Codec.Image.DevIL

-- | 'fmap' like operator over a 'Board'.

instance OFunctor Buffer where
  (<$>) f (Buffer ty low hi brd) = Buffer (typeO1 f ty) low hi (FmapBuffer f brd ty)

newBufferOf :: (Int,Int) -> (Int,Int) -> O a -> Buffer a
newBufferOf low hi a = Buffer (typeO a) low hi (BoardInBuffer (Board (typeO a) (PrimConst a)))

-- | read a file containing a common image format (jpg, gif, etc.), and create a 'Board RGBA', and the X and Y size of the image.
readBuffer :: String -> IO (Buffer (RGBA -> RGBA))
readBuffer filename = do
  arr <- readImage filename 
  let (low@(0,0,0), high@(h,w,3)) = U.bounds arr
  let bs = BS.pack [ arr U.! (x,y,z) | x <- [0..h], y <- [0..w], z <- [0..3]]
  return $ newBufferRGBA bs (w+1,h+1)

newBufferRGB :: ByteString -> (Int,Int) -> Buffer RGB
newBufferRGB bs (x,y) = Buffer RGB_Ty (0,0) (x-1,y-1) $ ImageRGB bs

newBufferRGBA :: ByteString -> (Int,Int) -> Buffer (RGBA -> RGBA)
newBufferRGBA bs (x,y) = Buffer RGBA_Ty (0,0) (x-1,y-1) $ ImageRGBA bs

-- | building a 2d byte buffer
newBufferUI :: ByteString -> (Int,Int) -> Buffer UI
newBufferUI bs (x,y) = Buffer UI_Ty (0,0) (x-1,y-1) $ ImageUI bs

bufferBounds :: Buffer a -> ((Int,Int),(Int,Int))
bufferBounds (Buffer _ low hi _) = (low,hi)

bufferSize :: Buffer a -> (Int,Int)
bufferSize (Buffer _ (x0,y0) (x1,y1) _) = (1+x1-x0,1+y1-y0)

-- how is this sampled? Is it supersampled?
boardToBuffer :: (Int,Int) -> (Int,Int) -> Board a -> Buffer a
boardToBuffer low high brd = Buffer (typeOfBoard brd) low high $ BoardInBuffer brd

moveBuffer :: (Int,Int) -> Buffer a -> Buffer a
moveBuffer (x,y) (Buffer ty (x0,y0) (x1,y1) buff) = Buffer ty (x0 + x,y0 + y) (x1 + x,y1 + y) buff