-- ChalkBoard Monad
-- October 2009
-- Kevin Matlage, Andy Gill


module Graphics.ChalkBoard.OpenGL.Monad where

import Graphics.ChalkBoard.CBIR( BufferId )
import Graphics.Rendering.OpenGL.Raw.Core31 as GL ( GLint, GLuint, GLenum )
import Foreign.Ptr ( Ptr )
import Data.Map ( Map )
import Control.Concurrent.MVar ( MVar, takeMVar, putMVar )





data CBM a = CBM { runCBM :: CBenv -> IO a }


instance Monad CBM where
    return n = CBM $ \_ -> return n
    m >>= k = CBM $ \env -> do
        a <- runCBM m env
        a' <- runCBM (k a) env
        return a'
    fail msg = CBM $ \_ -> fail msg


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






getCBMEnv :: CBM CBenv
getCBMEnv = CBM $ \env -> return env


getDebugFrames :: CBM (Bool)
getDebugFrames = CBM $ \env -> return (debugFrames env)

getDebugAll :: CBM (Bool)
getDebugAll = CBM $ \env -> return (debugAll env)

getDebugBoards :: CBM ([BufferId])
getDebugBoards = CBM $ \env -> return (debugBoards env)

getFBOSupport :: CBM (Bool)
getFBOSupport = CBM $ \env -> return (fboSupport env)






setCBMState :: CBstate -> CBM ()
setCBMState state = CBM $ \env -> do
        _ <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) state

getCBMState :: CBM CBstate
getCBMState = CBM $ \env -> do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        return (st)




setTexMap :: Map BufferId TextureInfo -> CBM ()
setTexMap texMap = CBM $ \env -> do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) (st {textureInfo = texMap})

getTexMap :: CBM (Map BufferId TextureInfo)
getTexMap = CBM $ \env -> do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        return (textureInfo st)


setCurrentBoard :: BufferId -> CBM ()
setCurrentBoard board = CBM $ \env -> do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) (st {currentBoard = board})

getCurrentBoard :: CBM BufferId
getCurrentBoard = CBM $ \env -> do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        return (currentBoard st)


setFBOPtr :: Ptr GL.GLuint -> CBM ()
setFBOPtr ptr = CBM $ \env -> do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) (st {fboPtr = ptr})

getFBOPtr :: CBM (Ptr GL.GLuint)
getFBOPtr = CBM $ \env -> do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        return (fboPtr st)





liftIO :: IO a -> CBM a
liftIO m = CBM $ \_ -> do
        a <- m
        return a


