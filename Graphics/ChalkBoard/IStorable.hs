{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Graphics.ChalkBoard.IStorable 
	( IStorableArray 	-- abstract
	, iStorableArray
	, (!)
	, bounds
	, rangeSize
	, withIStorableArray
	, touchIStorableArray
	, newIStorableArray
	) where
	
	
import qualified Data.Array.MArray as M	-- bogus warning, see OPTIONS
import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed (Ix, UArray)
import qualified Data.Ix as Ix
import qualified Data.Array.Storable as S
import Data.Array.Storable ( withStorableArray, StorableArray, unsafeFreeze, newArray_ )
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

-- | Number of bytes in the array
rangeSize :: (Ix i) =>  (IStorableArray i) -> Int
rangeSize (IStorableArray sa ua) = Ix.rangeSize (U.bounds ua)

-- | There is a rule that you can only read from this pointer, *never* write.
withIStorableArray :: IStorableArray i -> (Ptr Word8 -> IO a) -> IO a
withIStorableArray (IStorableArray sa _) k = S.withStorableArray sa k

touchIStorableArray :: IStorableArray i -> IO ()
touchIStorableArray (IStorableArray sa _) = S.touchStorableArray sa

newIStorableArray :: (Show i, Ix i) => (i,i) -> (Ptr Word8 -> IO ()) -> IO (IStorableArray i)
newIStorableArray arrBounds fn = do
	arr <- newArray_ arrBounds
            
            -- *Only* inside this callback can you safely write to this array.
	withStorableArray arr fn 

	iArr <- unsafeFreeze arr

	return $ IStorableArray arr iArr
	

