-- ChalkBoard Environment
-- October 2009
-- Kevin Matlage, Andy Gill


module Graphics.ChalkBoard.OpenGL.Env where

import Prelude hiding ( lookup )
import Graphics.ChalkBoard.CBIR( BufferId, StreamId, FragFunctionId )
import Graphics.ChalkBoard.Video ( OutPipe )
import Graphics.Rendering.OpenGL.Raw.Core31 as GL ( GLint, GLuint, GLenum )
import Graphics.Rendering.OpenGL
import Foreign.Ptr ( Ptr )
import Data.Map ( Map, insert, delete, lookup, notMember )
import Control.Concurrent.MVar ( MVar, takeMVar, putMVar )
import Data.IORef
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( when )




data CBenv = CBenv
        { debugFrames :: Bool
        , debugAll :: Bool
        , debugBoards :: [BufferId]
        , fboSupport :: Bool
        , envForStateVar :: MVar CBstate
        -- the variables
        , fracFunctionInfo :: IORef (Map FragFunctionId FragFunctionInfo)
        , currentFunction  :: IORef (Maybe FragFunctionId)
        , currentStream :: IORef (Maybe StreamId)
        }

{- Examples of using the fracFunctionInfo variable

   do x <- ...
      -- read the FFI value
      ffi <- get (fracFunctionInfo env)

      -- write to the FFI
      fracFunctionInfo env $= empty

      -- to modify the FFI
      fracFunctionInfo env $~ \ ffi -> insert ffi a b

NOTE: we use IORef because *all* the OpenGL code must be inside a single thread
-}


data CBstate = CBstate
        { currentBoard :: BufferId			-- The main drawing onto the screen (viewing) board
        , boundFBOBoard :: BufferId
        , fboPtr       :: Ptr GL.GLuint
        , textureInfo  :: Map BufferId TextureInfo
--      , fracFunctionInfo :: Map FragFunctionId FragFunctionInfo
--      , currentFunction :: Maybe FragFunctionId	-- Currently used fragment
        , outStreams :: Map StreamId OutPipe
        }

data TextureInfo = TextureInfo
	{ texPtr    :: Ptr GL.GLuint
	, texSize   :: (GL.GLint,GL.GLint)
	, texFormat :: GL.GLenum
	}

data FragFunctionInfo = FragFunctionInfo
        { ffUniform :: [String]		-- names of arguments
        , ffProg    :: Program		-- the program
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

setBoundFBOBoard :: CBenv -> BufferId -> IO ()
setBoundFBOBoard env board = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) (st {boundFBOBoard = board})

getBoundFBOBoard :: CBenv -> IO (BufferId)
getBoundFBOBoard env = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        return (boundFBOBoard st)

{-
setCurrentProgram :: CBenv -> Maybe FragFunctionId -> IO ()
setCurrentProgram env board = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) (st {currentFunction = board})

getCurrentProgram :: CBenv -> IO (Maybe FragFunctionId)
getCurrentProgram env = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        return (currentFunction st)
-}

setFBOPtr :: CBenv -> Ptr GL.GLuint -> IO ()
setFBOPtr env ptr = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) (st {fboPtr = ptr})

getFBOPtr :: CBenv -> IO (Ptr GL.GLuint)
getFBOPtr env = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        return (fboPtr st)


addOutStream :: CBenv -> StreamId -> OutPipe -> IO ()
addOutStream env sid opipe = do
        st <- takeMVar (envForStateVar env)
        let ostreams = outStreams st
            newOutStreams = (insert sid opipe ostreams)
        putMVar (envForStateVar env) (st {outStreams = newOutStreams})

rmOutStream :: CBenv -> StreamId -> IO ()
rmOutStream env sid = do
        st <- takeMVar (envForStateVar env)
        let ostreams = outStreams st
            newOutStreams = (delete sid ostreams)
        putMVar (envForStateVar env) (st {outStreams = newOutStreams})

getOutStream :: CBenv -> StreamId -> IO (OutPipe)
getOutStream env sid = do
        st <- takeMVar (envForStateVar env)
        putMVar (envForStateVar env) st
        let ostreams = outStreams st
        when (notMember sid ostreams) $ do
            print "Error: The specified output stream does not exist."
            exitWith (ExitFailure 1)
        let (Just outstream) = lookup sid ostreams
        return outstream



