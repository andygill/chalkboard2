-- ChalkBoard Environment
-- October 2009
-- Kevin Matlage, Andy Gill


module Graphics.ChalkBoard.OpenGL.Env where

import Graphics.ChalkBoard.CBIR( BufferId )
import Graphics.Rendering.OpenGL.Raw.Core31 as GL ( GLint, GLuint, GLenum )
import Foreign.Ptr ( Ptr )
import Data.Map ( Map )
import Control.Concurrent.MVar ( MVar, takeMVar, putMVar )




data CBenv = CBenv
        { debugFrames :: Bool
        , debugAll :: Bool
        , debugBoards :: [BufferId]
        , fboSupport :: Bool
        , envForStateVar :: MVar CBstate
        }

data CBstate = CBstate
        { currentBoard :: BufferId
        , textureInfo  :: Map BufferId TextureInfo
        , fboPtr       :: Ptr GL.GLuint
        }


data TextureInfo = TextureInfo
	{ texPtr    :: Ptr GL.GLuint
	, texSize   :: (GL.GLint,GL.GLint)
	, texFormat :: GL.GLenum
	}




getDebugFrames :: CBenv -> IO (Bool)
getDebugFrames env = return (debugFrames env)

getDebugAll :: CBenv -> IO (Bool)
getDebugAll env = return (debugAll env)

getDebugBoards :: CBenv -> IO ([BufferId])
getDebugBoards env = return (debugBoards env)

getFBOSupport :: CBenv -> IO (Bool)
getFBOSupport env = return (fboSupport env)




setCBMState :: CBenv -> CBstate -> IO ()
setCBMState env state = do
        _ <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) state

getCBMState :: CBenv -> IO (CBstate)
getCBMState env = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        return (st)



setTexMap :: CBenv -> Map BufferId TextureInfo -> IO ()
setTexMap env texMap = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) (st {textureInfo = texMap})

getTexMap :: CBenv -> IO (Map BufferId TextureInfo)
getTexMap env = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        return (textureInfo st)


setCurrentBoard :: CBenv -> BufferId -> IO ()
setCurrentBoard env board = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) (st {currentBoard = board})

getCurrentBoard :: CBenv -> IO (BufferId)
getCurrentBoard env = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        return (currentBoard st)


setFBOPtr :: CBenv -> Ptr GL.GLuint -> IO ()
setFBOPtr env ptr = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) (st {fboPtr = ptr})

getFBOPtr :: CBenv -> IO (Ptr GL.GLuint)
getFBOPtr env = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        return (fboPtr st)



