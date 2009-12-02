{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Graphics.ChalkBoard.IStorable 
	( IStorableArray 	-- abstract
	, iStorableArray
	, (!)
	, bounds
	, withIStorableArray
	, touchIStorableArray
	) where
	
	
import qualified Data.Array.MArray as M	-- bogus warning, see OPTIONS
import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed (Ix, UArray)
import qualified Data.Array.Storable as S
import Foreign.Ptr
import Data.Word
import Data.Binary
import System.IO.Unsafe

-- This is a create one, read many times array, that is also
-- storable, that is you can get a 'Ptr' to it, provided you
-- promise not to change the (contents of the) Ptr.

data IStorableArray i = IStorableArray (S.StorableArray i Word8) (UArray i Word8)

instance (Ix ix, Binary ix) => Binary (IStorableArray ix) where
   put isa@(IStorableArray _ arr) = put arr
   get = do arr <- get
	    return $ unsafePerformIO $ iStorableArray arr

iStorableArray :: (Ix i) => UArray i Word8 -> IO (IStorableArray i)
iStorableArray arr = do
  arrT <- M.unsafeThaw arr
  return (IStorableArray arrT arr)
	
(!) :: (Ix i) => IStorableArray i -> i -> Word8
(!) (IStorableArray _ arr) ix = arr U.! ix

bounds :: (Ix i) => (IStorableArray i) -> (i,i)
bounds (IStorableArray sa ua) = U.bounds ua

withIStorableArray :: IStorableArray i -> (Ptr Word8 -> IO a) -> IO a
withIStorableArray (IStorableArray sa _) k = S.withStorableArray sa k

touchIStorableArray :: IStorableArray i -> IO ()
touchIStorableArray (IStorableArray sa _) = S.touchStorableArray sa
