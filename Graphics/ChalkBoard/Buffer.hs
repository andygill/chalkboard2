{-# LANGUAGE TypeFamilies, ExistentialQuantification, GADTs, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module Graphics.ChalkBoard.Buffer 
	( -- * The 'Board' 
	  Buffer
	  -- * Ways of manipulating 'Buffer'.
	, (<$>)
	  -- * Ways of looking at a Buffer
	, bufferBounds
	  -- * Ways of creating a new 'Board'.
	, newBufferOf
	, readBuffer
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
import Codec.Image.DevIL

-- | 'fmap' like operator over a 'Board'.

instance OFunctor Buffer where
  (<$>) f (Buffer low hi brd) = Buffer low hi (FmapBuffer f brd)

newBufferOf :: (Int,Int) -> (Int,Int) -> O a -> Buffer a
newBufferOf low hi a = Buffer low hi (BoardInBuffer (PrimConst a))

-- | read a file containing a common image format (jpg, gif, etc.), and create a 'Board RGBA', and the X and Y size of the image.
readBuffer :: String -> IO (Buffer RGBA)
readBuffer filename = do
  arr <- readImage filename 
  iStore <- iStorableArray arr 
  let ((0,0,0), (h,w,3)) = U.bounds arr
  return $ Buffer (0,0) (h,w) (Image iStore)

newBufferRGB :: IS.IStorableArray (Int,Int,Int) -> Buffer RGB
newBufferRGB iStore = Buffer (l1,l2) (h1,h2) $ Image iStore
    where ((l1,l2,0),(h1,h2,2)) = IS.bounds iStore
	

bufferBounds :: Buffer a -> ((Int,Int),(Int,Int))
bufferBounds (Buffer low hi _) = (low,hi)


