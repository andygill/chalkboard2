-- ChalkBoard Options
-- October 2009
-- Kevin Matlage, Andy Gill


module Graphics.ChalkBoard.Options where

import Graphics.ChalkBoard.CBIR( BufferId )
import Data.Binary
import Control.Monad


data Options = NoFBO
             | DebugFrames
             | DebugAll				-- ^ not supported (yet!)
             | DebugBoards [BufferId]		-- ^ not supported (yet!)
	     | BoardSize Int Int		-- ^ default is 400x400.
	     | FullScreen			-- ^ not supported (yet!)
	     | DebugCBIR
	     | VerboseVideo
        deriving (Eq, Show)

instance Binary Options where
  put (NoFBO) 	 		 = put (0 :: Word8)
  put (DebugFrames) 	 	 = put (1 :: Word8)
  put (DebugBoards buffs)        = put (2 :: Word8) >> put buffs
  put (BoardSize w h) 		 = put (3 :: Word8) >> put w >> put h
  put (FullScreen) 		 = put (4 :: Word8)
  put (DebugCBIR) 		 = put (5 :: Word8)
  put (VerboseVideo) 		 = put (6 :: Word8)

  get = do tag <- getWord8
           case tag of
                  0 -> return $ NoFBO 
                  1 -> return $ DebugFrames
                  2 -> liftM DebugBoards get
		  3 -> liftM2 BoardSize get get
		  4 -> return $ FullScreen
		  5 -> return $ DebugCBIR
		  5 -> return $ VerboseVideo
