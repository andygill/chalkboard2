-- {-# OPTIONS_GHC -ddump-simpl-stats #-}

-- ChalkBoard Back End
-- August 2009
-- Kevin Matlage, Andy Gill


module Graphics.ChalkBoard.OpenGL.CBBE where


-- ChalkBoard or Non-Standard Packages
import Graphics.ChalkBoard.CBIR as CBIR
import Graphics.ChalkBoard.IStorable as IS
import Graphics.ChalkBoard.Types as T (RGB(..),RGBA(..))
import Graphics.ChalkBoard.OpenGL.Monad
import Graphics.ChalkBoard.Options

import Graphics.UI.GLUT hiding ( GLuint, GLint, GLfloat )
import qualified Graphics.UI.GLUT as GLUT 
import Graphics.Rendering.OpenGL.Raw.Core31 as GL
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (gl_LUMINANCE)
import Codec.Image.DevIL

-- Base Packages
import Prelude hiding ( lookup )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, tryTakeMVar, takeMVar, putMVar )
import Control.Monad ( when )
import Foreign.Ptr ( Ptr, nullPtr, castPtr )
import Foreign.C.Types ( CUChar )
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Storable ( peek )
import Data.Map ( Map, empty, insert, delete, lookup, notMember )
import Data.Maybe ( fromMaybe )
import Data.Array.Unboxed as U  
import Data.Array.Storable ( withStorableArray, StorableArray )
import Data.Array.MArray ( unsafeThaw, newArray_, MArray )
import System.Exit ( exitWith, ExitCode(..) )


-- Debugging Packages
import System.IO ( writeFile, appendFile )
import System.Directory ( removeFile, doesFileExist )
import Data.Unique
--import Data.Time.Clock


startRendering :: BufferId -> MVar () -> MVar ([Inst BufferId]) -> [Options] -> IO()
startRendering board booted insts options = do

    -- Take the initial CBIR instructions out of the mvar
    initChanges <- takeMVar insts
	
    let (x,y) = case initChanges of
	       [Allocate viewBoard (w,h) _ _] | viewBoard == board -> (fromIntegral w,fromIntegral h)
	       _ -> error $ "Opps: strange bootstrapping code: " ++ show initChanges

    -- Init GLUT and take/apply any command line arguments that pertain to it or X windows
    getArgsAndInitialize
    
    -- Select display mode: Double buffered, RGBA,  Alpha components, Depth buffer
    initialDisplayMode $= [  DoubleBuffered, RGBAMode, WithAlphaComponent, WithDepthBuffer ]
    -- Get an 800x600 window. Should we change the default?
    initialWindowSize $= Size x y
    -- Start the window in upper left corner of the screen
    initialWindowPosition $= Position 0 0
    -- Open the window
    createWindow "ChalkBoard"
    
    -- Initialize some OpenGL settings and features.
    initGL
    -- Also initialize devIL for importing/exporting images
    ilInit
    
    -- Initialize the ChalkBoard Monad state/environment
    let state = initCBMState board
    env <- initCBMEnv options state
    
    let debug = (debugFrames env)
    when (debug) $
        writeFile "./debug.html" "<HTML>\n<TITLE>ChalkBoard Debugging</TITLE>\n<br>\n"

    -- See which version of OpenGL is being used.
    (major,minor) <- get (majorMinor glVersion)
    print $ "OpenGL Version: " ++ (show major) ++ "." ++ (show minor)
    
    -- See if one of the fbo extensions is supported
    extensions <- get glExtensions
    let fboExtension = ("GL_EXT_framebuffer_object" `elem` extensions || "GL_ARB_framebuffer_object" `elem` extensions)
    
    fboOn <- if (fboSupport env == True && (major >= 3 || fboExtension))
                 then do
                     -- Initialize a FBO
                     (fboIdPtr, texIdPtr) <- initFBO -- The returned FBO is still bound as the current framebuffer
                     -- Check if FBOs are supported
                     status <- glCheckFramebufferStatus gl_FRAMEBUFFER
                     print $ "FBO Unsupported?: " ++ (show (status == gl_FRAMEBUFFER_UNSUPPORTED))

                     -- Depending on whether they're supported or not, determine whether FBOs should be used
                     complete <- if (status == gl_FRAMEBUFFER_COMPLETE)
                                     then do
                                         print "FBO Initialization Complete. Using FBOs."
                                         st <- takeMVar (envForStateVar env)
                                         putMVar (envForStateVar env) (st {fboPtr = fboIdPtr})
                                         return True
                                     else do
                                         print "FBO Initialization Incomplete. Not Using FBOs."
                                         glDeleteFramebuffers 1 fboIdPtr -- Delete the FBO since it isn't being used
                                         return False

                     -- Delete the texture that was just used to test if FBOs were supported
                     glDeleteTextures 1 texIdPtr
                     -- Return whether the FBO initialization was complete
                     return complete
                 else do
                     print "FBOs Not Supported. Not Using FBOs."
                     return False

    -- Start the changeboard timer callback, which will execute all CBIR instructions that are passed in
    runCBM (changeBoard' (changeBoard insts)  initChanges) (env {fboSupport = fboOn})

    -- Register the function called when the window is resized
    reshapeCallback $= Just resizeScene
    -- Register the function called when the keyboard is pressed.
    keyboardMouseCallback $= Just (keyPressed debug)
    
    -- Start the main GLUT event loop after telling the front end that OpenGL has been booted
    flush
    putMVar booted ()
    mainLoop






-- Function to initialize the state of the CBBE
initCBMState :: BufferId -> CBstate
initCBMState board = CBstate board (empty::Map BufferId TextureInfo) nullPtr


-- TODO: add option for verboseness
--Funciton to initialize the environment of the CBBE monad
initCBMEnv :: [Options] -> CBstate -> IO ( CBenv )
initCBMEnv options state = do
        let fboSupport' = not $ NoFBO `elem` options
            debugFrames' = DebugFrames `elem` options
            debugAll' = DebugAll `elem` options
            debugBoards' = concat [ids | DebugBoards ids <- options]
        v <- newEmptyMVar
        putMVar v state
        return $ CBenv debugFrames' debugAll' debugBoards' fboSupport' v






-- Function to initialize some OpenGL settings and features
initGL :: IO ()
initGL = do
    clearColor $= Color4 1 1 1 1 -- Clear the background color to white
    
    -- Not sure if a couple of the things in this block are really needed
    clearDepth $= 1 -- Enables clearing of the depth buffer
    depthFunc  $= Just Less -- Type of depth test
    shadeModel $= Smooth -- Enables smooth color shading
    polygonMode $= (Fill,Fill)
    
    -- Blending and texture functions that make chalkboard work correctly
    blend $= Enabled
    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, OneMinusSrcAlpha)) -- Specify color and alpha blend separately
    texture Texture2D $= Enabled
    textureFunction $= Replace --Replace destination color and alpha with texture's color and alpha
    
    Size width height <- get windowSize -- Get the size of the window
    resizeScene (Size width height) -- Resize the viewport and projection
    





-- Will possibly want to change this from using the window w/h to something else (maintain ratio?)
-- Reshape callback function to resize the viewing area appropriately when the window is resized.
resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevents divide by zero
resizeScene s@(Size width height) = do
    let w = fromIntegral width
        h = fromIntegral height
    viewport   $= (Position 0 0, Size (width*1) (height*1))	-- the whole window is used
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (w*1) 0 (h*1) -- Will probably want to change this from using the window w/h
    matrixMode $= Modelview 0
    flush -- Might not be necessary
    postRedisplay Nothing






-- Keyboard and Mouse callback function
-- Right now just exits the program when the escape key is pressed.
keyPressed :: Bool -> KeyboardMouseCallback
keyPressed debug (Char '\27') Down _ _ = do
    when (debug) $ appendFile "./debug.html" "</HTML>"
    exitWith ExitSuccess -- 27 is ESCAPE
keyPressed _     _            _    _ _ = return ()






-- Function to initiallize a framebuffer object (FBO) so that we can know whether FBOs are supported.
-- Will also possibly want to just test the OpenGL version string as an initial check before wasting the time to do this.
initFBO :: IO( (Ptr GLuint, Ptr GLuint) )
initFBO = do
    -- Create a Framebuffer object and bind it
    fboIdPtr <- malloc :: IO(Ptr GLuint)
    glGenFramebuffers 1 fboIdPtr
    fboId <- peek fboIdPtr
    glBindFramebuffer gl_FRAMEBUFFER fboId
    
    let w = 1
        h = 1
    
    {- 
    -- Create a renderbuffer object to store depth info
    rboIdPtr <- malloc :: IO(Ptr GLuint)
    glGenRenderbuffers 1 rboIdPtr
    rboId <- peek rboIdPtr
    glBindRenderbuffer gl_RENDERBUFFER rboId
    glRenderbufferStorage gl_RENDERBUFFER gl_DEPTH_COMPONENT w h
    glBindRenderbuffer gl_RENDERBUFFER 0
    -- Attach the renderbuffer to the FBO depth attachment point
    glFramebufferRenderbuffer gl_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_RENDERBUFFER rboId
    --}

    -- Create a texture object and bind it    NEED: to abstract out the color (RGBA)
    texIdPtr <- malloc :: IO(Ptr GLuint)
    glGenTextures 1 texIdPtr
    texId <- peek texIdPtr
    glBindTexture gl_TEXTURE_2D texId  
      
    --Set up the texture object and its parameters
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR
    --glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR_MIPMAP_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE 
    glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGBA8) w h 0 gl_RGBA gl_UNSIGNED_BYTE nullPtr
    
    -- Unbind this texture so it isn't the one currently being used
    glBindTexture gl_TEXTURE_2D 0
    -- Attach the texture to a FBO color attachment point
    bindFrameBufferToTexture texId (w,h)
    -- glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D texId 0
    
    return (fboIdPtr, texIdPtr)






-- We need to come up with a better name for this function
-- Now, it only works *if* you already have a set of instructions to read.
changeBoard' :: CBM () -> [Inst BufferId] -> CBM ()
changeBoard' next changes = do
--    print "changeBoard"
    {-
    when (not (null changes)) $ do
       print changes
    --}
    --Draw all new instructions (into the textures with ptrs stored in the map)
    --tm <- liftIO $ getCurrentTime
    drawInsts changes
    --tm' <- liftIO $ getCurrentTime
    {-
    liftIO $ when (not (null changes)) $ do
        print (diffUTCTime tm' tm)
    --}
    
    debug <- getDebugFrames
    curBoard <- getCurrentBoard
    env <- getCBMEnv
    
    liftIO $ do
        temp <- get errors
        when (not (null temp)) $ 
    	    print ("ERRORS",temp)
    
        when (debug && (not (null changes))) $ do
            imgUnique <- newUnique
            imgNum <- return $ hashUnique imgUnique
            appendFile "./debug.html" $ "<br><pre>" ++ showCBIRs changes ++ "</pre>\n"
            appendFile "./debug.html" $ "<br>\n<img src=\"debug-" ++ show imgNum ++ ".png\"/>\n<br>\n<br>\n<br>\n<hr>\n<br>\n<br>\n"
            alreadyExists <- doesFileExist $ "./debug-" ++ show imgNum ++ ".png"
            when (alreadyExists) $ removeFile ("./debug-" ++ show imgNum ++ ".png")
            runCBM (saveImage curBoard ("./debug-" ++ show imgNum ++ ".png")) env
    
        addTimerCallback 20 $ runCBM next env
        displayCallback $= runCBM (drawBoard) env
        postRedisplay Nothing



-- Function to see if there were any new instructions passed in and if so to call the changeboard' function
--   * insts - An MVar possibly containing a list of CBIR instructions to make changes to the boards
changeBoard :: MVar ([Inst BufferId]) -> CBM ()
changeBoard insts = do
--    print "changeBoard"

    maybeInsts <- liftIO $ do 
                        mbInsts <- tryTakeMVar insts
                        return mbInsts
        
    let changes = fromMaybe [] maybeInsts

    changeBoard' (changeBoard insts) changes






-- Display callback function to make sure the right framebuffer is bound and display the final display board.
drawBoard :: CBM ()
drawBoard = do
--    liftIO $ print "drawBoard"
 
    -- First, see if we are using a FBO or not
    fboSupp <- getFBOSupport

    if fboSupp
        then do
            -- If using a FBO, get the ptr so we can reset it after displaying
            fboIdPtr <- getFBOPtr
            liftIO $ do
                -- Reset the framebuffer to the actual window
                glBindFramebuffer gl_FRAMEBUFFER 0
            -- Draw the final board
            displayBoard
            liftIO $ do
                -- Change the framebuffer back so that we can start drawing boards again
                flush
                swapBuffers
                fboId <- peek fboIdPtr
                glBindFramebuffer gl_FRAMEBUFFER fboId
        else do
            displayBoard
            liftIO $ do    
                flush
                swapBuffers




-- Function to display the current output board of a chalkboard image. Done once per frame.
displayBoard :: CBM ()
displayBoard = do
    texMap <- getTexMap
    b <- getCurrentBoard
    
    liftIO $ do
            -- Check to make sure the display board exists
            when (notMember b texMap) $ do
                    print "Error: The board to display doesn't exist."
                    exitWith (ExitFailure 1)
            
            -- Get some info about the current output board
            let (Just texInfo) = lookup b texMap
                texIdPtr = texPtr texInfo
                (w',h') = texSize texInfo
                (w,h) = (fromIntegral w', fromIntegral h')
            
            texId <- peek texIdPtr
            
            {- Turn this off to center the image instead of snapping the window to its size
            Size winW winH <- get windowSize
            when (winW /= fromIntegral w || winH /= fromIntegral h) $
                if (w < 200)
                    then do 
                        windowSize $= (Size 200 h)
                        resizeScene (Size 200 h)
                    else do
                        windowSize $= (Size w h)
                        resizeScene (Size w h)
            --}
            
            -- Calculations to center the image in the window
            Size winW2 winH2 <- get windowSize -- Get the size of the window    
            let minW = ((fromIntegral (fromIntegral winW2 -  w)) :: GLfloat) / 2.0
                minH = ((fromIntegral (fromIntegral winH2 - h)) :: GLfloat) / 2.0
                maxW = (fromIntegral winW2 - minW) 
                maxH = (fromIntegral winH2 - minH) 
            
            -- Bind the texture so that we can display it
            glBindTexture gl_TEXTURE_2D texId
            
    	    clearColor $= Color4 1 1 1 1 -- Clear the background color to white
            clear [ColorBuffer, DepthBuffer] -- clear the screen and the depth buffer
            loadIdentity -- reset view
            color (Color4 1 1 1 (1::GL.GLfloat))
            -- Display the final board, making sure that it is drawn in the upper left corner of the window (for now)
            renderPrimitive Quads $ do
                texCoord (TexCoord2 0 (1::GL.GLfloat)) -- Top Left
                vertex (Vertex3 minW maxH 0)
                texCoord (TexCoord2 0 (0::GL.GLfloat)) -- Bottom Left
                vertex (Vertex3 minW minH 0) -- Used to be GL.GLsizei
                texCoord (TexCoord2 1 (0::GL.GLfloat)) -- Bottom Right
                vertex (Vertex3 maxW minH 0)
                texCoord (TexCoord2 1 (1::GL.GLfloat)) -- Top Right
                vertex (Vertex3 maxW maxH 0) 
            
            -- Unbind the texture in case we need to keep writing to it later
            glBindTexture gl_TEXTURE_2D 0








-- Function to loop (recurse) through CBIR instructions and apply all of their effects to change or create boards
--   * (i:is)  - The list of CBIR instructions that are left to execute
drawInsts :: [Inst BufferId] -> CBM ()
drawInsts [] = return ()
drawInsts (i:is) = do 
    case i of
            (Allocate b size depth (BackgroundArr arr)) -> allocateArrBuffer b size depth arr
            (Allocate b size depth bgColor) -> allocateBuffer b size depth bgColor
            (AllocateImage b imagePath ) -> allocateImgBuffer b imagePath
            (SplatTriangle bSource bDest ptMap1 ptMap2 ptMap3) -> splatPolygon bSource bDest [ptMap1, ptMap2, ptMap3]
            (SplatPolygon bSource bDest ptMaps) -> splatPolygon bSource bDest ptMaps
            (SplatColor sColor bDest useBlend ptList) -> splatColor sColor bDest useBlend ptList
            (SplatBuffer bSource bDest) -> splatPolygon bSource bDest [ PointMap p p | p <- [(0,0),(0,1),(1,1),(1,0)] ]
            (CopyBuffer alpha bSource bDest) -> copyBuffer alpha bSource bDest
            (SaveImage b savePath) -> saveImage b savePath
            (Delete b) -> deleteBuffer b
            (Nested _ insts') -> drawInsts insts'
            (CBIR.Exit)  -> liftIO $ exitWith ExitSuccess 
    drawInsts is








-- Function to allocate a new board/buffer object
--   * board - The buffer (board) object name
--   * (w,h) - The width and height of the new buffer being created
--   * d - The color depth of the buffer being created
--   * c - The initial color of the buffer that is being created
allocateBuffer :: BufferId -> (Int,Int) -> Depth -> Background -> CBM ()
allocateBuffer board (w,h) d c = do
    fboSupp <- getFBOSupport
    texMap <- getTexMap
    
    texInfo' <- liftIO $ do
            -- Choose the internal format to use for this buffer based on the depth specified
            let colorType = case d of
                    BitDepth -> (fromIntegral gl_LUMINANCE)   -- 8 bit per pixel (still grey, not just black and white)
                    G8BitDepth -> (fromIntegral gl_LUMINANCE) -- 8 bits per pixel (grey)
                    RGB24Depth -> (fromIntegral gl_RGB) -- (R,G,B), 8 bits per pixel
                    RGBADepth -> (fromIntegral gl_RGBA) -- (R,G,B,A), 8 bits per pixel
            
            -- Choose the initial background color of the buffer based on the background specified
            let bgcolor = case c of 
                    (BackgroundBit on) -> if on then (Color4 0 0 0 1) else (Color4 1 1 1 1) 
                    (BackgroundG8Bit grey) -> (Color4 (floatToGLclampf grey) (floatToGLclampf grey) (floatToGLclampf grey) 1)
                    (BackgroundRGB24Depth (T.RGB r g b)) -> (Color4 (floatToGLclampf r) (floatToGLclampf g) (floatToGLclampf b) 1)
                    (BackgroundRGBADepth (T.RGBA r g b a)) -> (Color4 (floatToGLclampf r) (floatToGLclampf g) (floatToGLclampf b) (floatToGLclampf a))
            
            -- Create a texture object and bind it
            texIdPtr <- malloc :: IO(Ptr GLuint)
            glGenTextures 1 texIdPtr
            texId <- peek texIdPtr
            glBindTexture gl_TEXTURE_2D texId
               
            --Set up the texture object and its parameters
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR
            --glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR_MIPMAP_LINEAR --Can maybe check into this now that generation is only done once
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
            
            let texInfo = TextureInfo texIdPtr (fromIntegral w, fromIntegral h) colorType
            
            when (fboSupp) $ do
                -- Set up the texture so that it's image can be stored when drawing to the framebuffer
                -- colorType for both?
                glTexImage2D gl_TEXTURE_2D 0 (fromIntegral colorType) (fromIntegral w) (fromIntegral h) 0 (fromIntegral colorType) gl_UNSIGNED_BYTE nullPtr 
                -- Attach the texture to a FBO color attachment point
		bindFrameBufferToTexture texId (w,h)
                -- glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D texId 0
            
--            preservingAttrib [ColorBufferAttributes] $ do --Temporarily change the clear color to make the buffer
	    do  clearColor $= bgcolor -- Change the clearColor to the color of the board being created
                clear [ColorBuffer] -- Clear the screen to the new color to draw that color onto the board
                
            when (not fboSupp) $ do    
                -- Copy the texture from the framebuffer
                glCopyTexImage2D gl_TEXTURE_2D 0 colorType 0 0 (fromIntegral w) (fromIntegral h) 0
            
            -- Unbind Texture until it is needed (may want to take this out depending on how we order instructions coming in)
            glBindTexture gl_TEXTURE_2D 0
            return texInfo
    
    -- FBO is NOT unbound, nor is the texture image detached from the FBO
    setTexMap (insert board texInfo' texMap)




{- I've cut and pasted this from allocateImgBuffer -}
-- TODO: merge with function allocateArrBuffer, because allocateRawImgBuffer is only called in one place
allocateRawImgBuffer :: BufferId -> (Int,Int) -> Depth -> Ptr CUChar -> CBM ()
allocateRawImgBuffer board (w,h) depth imagePtr = do
    fboSupp <- getFBOSupport
    texMap <- getTexMap
    
    texInfo' <- liftIO $ do
            -- Just set the colorType to RGBA for now, this should maybe change so that they can use any format of image data 
            let colorType = case depth of
                    BitDepth -> (fromIntegral gl_LUMINANCE)   -- 8 bit per pixel (still grey, not just black and white)
                    G8BitDepth -> (fromIntegral gl_LUMINANCE) -- 8 bits per pixel (grey)
                    RGB24Depth -> (fromIntegral gl_RGB) -- (R,G,B), 8 bits per pixel
                    RGBADepth -> (fromIntegral gl_RGBA) -- (R,G,B,A), 8 bits per pixel

            -- Create a texture object and bind it
            texIdPtr <- malloc :: IO(Ptr GLuint)
            glGenTextures 1 texIdPtr
            texId <- peek texIdPtr
            glBindTexture gl_TEXTURE_2D texId
            
            --Set up the texture object and its parameters
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR
            --glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR_MIPMAP_LINEAR --Can maybe check into this now that generation is only done once
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
            
            glTexImage2D gl_TEXTURE_2D 0 (fromIntegral colorType) (fromIntegral w) (fromIntegral h) 0 (fromIntegral colorType) gl_UNSIGNED_BYTE (castPtr imagePtr)
            
            let texInfo = TextureInfo texIdPtr (fromIntegral w, fromIntegral h) colorType

            -- Unbind this texture so it isn't the one currently being used
            glBindTexture gl_TEXTURE_2D 0
            
            -- Done to mirror the other allocates (leaving the texture attached to the fbo), but should maybe just get rid of this:
            when (fboSupp) $ do
                    -- Attach the texture to a FBO color attachment point
		    bindFrameBufferToTexture texId (w,h)
                    -- glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D texId 0
            
            return texInfo

    -- FBO is NOT unbound, nor is the texture image detached from the FBO
    setTexMap (insert board texInfo' texMap)




allocateArrBuffer :: BufferId -> (Int,Int) -> Depth -> ReadOnlyCByteArray -> CBM ()
allocateArrBuffer board (w,h) depth imageArr = do
        env <- getCBMEnv
	liftIO $ IS.withReadOnlyCByteArray imageArr $ \p -> do
	        runCBM (allocateRawImgBuffer board (w,h) depth (castPtr p)) env




-- Function to allocate a new board/buffer object using a pre-existing image
--   * board - The buffer (board) object name
--   * imagePath - The path to the image file being loading into this new buffer
allocateImgBuffer :: BufferId -> FilePath -> CBM ()
allocateImgBuffer board imagePath = do
    fboSupp <- getFBOSupport
    texMap <- getTexMap
    
    texInfo' <- liftIO $ do
            -- Just set the colorType to RGBA for now
            let colorType = gl_RGBA

            -- Create a texture object and bind it
            texIdPtr <- malloc :: IO(Ptr GLuint)
            glGenTextures 1 texIdPtr
            texId <- peek texIdPtr
            glBindTexture gl_TEXTURE_2D texId
            
            --Set up the texture object and its parameters
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR
            --glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR_MIPMAP_LINEAR --Can maybe check into this now that generation is only done once
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
            
            -- Read in the image to an array from the filepath that was given using devIL
            arr <- readImage imagePath
            
            --tm2 <- getCurrentTime
            -- Get the array data and give that to an openGL texture
            let ((0,0,0), (h,w,3)) = U.bounds arr
            arrT <- unsafeThaw arr
            --tm2' <- getCurrentTime
            --print (diffUTCTime tm2' tm2)

            withStorableArray arrT $ \ptr -> do
                -- Might have to just do RGBA instead of colorType!!!
                glTexImage2D gl_TEXTURE_2D 0 (fromIntegral colorType) (fromIntegral w+1) (fromIntegral h+1) 0 (fromIntegral colorType) gl_UNSIGNED_BYTE (castPtr ptr)
            
            let texInfo = TextureInfo texIdPtr (fromIntegral w+1, fromIntegral h+1) colorType
            
            -- Unbind this texture so it isn't the one currently being used
            glBindTexture gl_TEXTURE_2D 0
            
            -- Done to mirror the other allocates (leaving the texture attached to the fbo), but should maybe just get rid of this:
            when (fboSupp) $ do
                    -- Attach the texture to a FBO color attachment point
		    bindFrameBufferToTexture texId (w,h)
                    -- glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D texId 0
            
            return texInfo

    -- FBO is NOT unbound, nor is the texture image detached from the FBO
    setTexMap (insert board texInfo' texMap)








-- Function to splat a polygon from one source buffer to one destination buffer
--   * bS - The source buffer (board) object name
--   * bD - The destination buffer (board) object name
--   * ps - A list of PointMaps, which specify a pairing of points: one on the source buffer that correspond one on the destination buffer
splatPolygon :: BufferId -> BufferId -> [PointMap] -> CBM ()
splatPolygon bS bD ps = do
    fboSupp <- getFBOSupport
    texMap <- getTexMap

    liftIO $ do
            -- Check to make sure both the source and destination boards exist
            when (notMember bD texMap) $ do
                    print "Error: The destination board to splat to doesn't exist."
                    exitWith (ExitFailure 1)
            when (notMember bS texMap) $ do
                    print "Error: The source board to splat doesn't exist."
                    exitWith (ExitFailure 1)
            
            -- Look up all of the values that will be needed
            let (Just texInfoD) = lookup bD texMap
                (Just texInfoS) = lookup bS texMap
                texIdPtrD = texPtr texInfoD
                texIdPtrS' = texPtr texInfoS
                (w,h) = texSize texInfoD
                colorType = texFormat texInfoD
            
            texIdD <- peek texIdPtrD
            texIdS' <- peek texIdPtrS'
            
            if (not fboSupp)
                then do
                    clear [DepthBuffer] -- clear the depth buffer
                    loadIdentity
                    --Bind the destination texture to use first
                    glBindTexture gl_TEXTURE_2D texIdD
                    -- Turn off blending so the destination board isn't blended with the background color
                    blend $= Disabled
                    -- Render the destination board so we can draw onto it
                    renderPrimitive Quads $ do
                        texCoord (TexCoord2 0 (0::GL.GLfloat)) -- Bottom Left
                        vertex (Vertex3 0 0 (0::GL.GLfloat)) -- Used to be GLUT.GLsizei, does it matter?
                        texCoord (TexCoord2 1 (0::GL.GLfloat)) -- Bottom Right
                        vertex (Vertex3 (fromIntegral w) 0 (0::GL.GLfloat))
                        texCoord (TexCoord2 1 (1::GL.GLfloat)) -- Top Right
                        vertex (Vertex3 (fromIntegral w) (fromIntegral h) (0::GL.GLfloat))
                        texCoord (TexCoord2 0 (1::GL.GLfloat)) -- Top Left
                        vertex (Vertex3 0 (fromIntegral h) (0::GL.GLfloat))
                    -- Turn blending back on so that the source board can be blended with the destination board
                    blend $= Enabled
                    
                    -- Bind the source texture that will be splatted on    
                    glBindTexture gl_TEXTURE_2D texIdS'
                    --Uses relative positions (percentages) of source and destination boards
                    renderPrimitive Polygon $
                        placeVerticies w h ps

                    -- Bind the destination texture so we can copy the new image out to it
                    glBindTexture gl_TEXTURE_2D texIdD
                    -- Copy the texture from the framebuffer (make more efficient by only copying the changed subimage?)
                    glCopyTexImage2D gl_TEXTURE_2D 0 (fromIntegral colorType) 0 0 (fromIntegral w) (fromIntegral h) 0 
                    
                else do

                    -- Attach the texture to a FBO color attachment point
		    bindFrameBufferToTexture texIdD (w,h)
                    -- glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D texIdD 0
                    -- Check to see if the texture is trying to recursively draw onto itself, and if so create a copy of the source texture
                    -- to prevent the undefined feedback loop that would result from drawing straight to the same texture that is being read
                    (texIdS, texIdPtrS) <- if (texIdD == texIdS')
                                               then fixTexLoopback texInfoS --Could call after binding texIdS' to avoid an extra binding or two maybe?
                                               else return (texIdS', texIdPtrS')
            
                    -- Bind the source texture that will be splatted on    
                    glBindTexture gl_TEXTURE_2D texIdS
                    --Uses relative positions (percentages) of source and destination boards
                    renderPrimitive Polygon $
                        placeVerticies w h ps  

                    -- If a new source texture was created to prevent a feedback loop, then delete it
                    when (texIdS /= texIdS') $ do
                            glDeleteTextures 1 texIdPtrS

            -- Unbind Texture until it is needed (may want to take this out depending on how we order instructions coming in)
            glBindTexture gl_TEXTURE_2D 0






-- Function to place all of the tex coords and verticies of a polygon splat from a list of PointMaps.
--   * w  - The width of the destination board.
--   * h  - The height of the destination board.
--   * ps - The list of PointMaps from the source board onto the destination board.
placeVerticies :: GLint -> GLint -> [PointMap] -> IO () -- Could maybe just make the first two as type 'a', which would be an integral
placeVerticies _ _ [] = return ()
placeVerticies w h (p:ps) = do
    let (PointMap (sx,sy) (dx,dy)) = p
    
    texCoord (TexCoord2 (floatToGLfloat sx) ((floatToGLfloat sy)::GL.GLfloat))
    vertex (Vertex3 (fromIntegral w * floatToGLfloat dx) (fromIntegral h * floatToGLfloat dy) (1::GL.GLfloat))
    
    placeVerticies w h ps


-- Function to place all of the verticies of a color splat from a list of PointMaps.
--   * w  - The width of the destination board.
--   * h  - The height of the destination board.
--   * ps - The list of UIPoints to splat a color shape onto the destination board.
placeColorVerticies :: GLint -> GLint -> [UIPoint] -> IO () -- Could maybe just make the first two as type 'a', which would be an integral
placeColorVerticies _ _ [] = return ()
placeColorVerticies w h (p:ps) = do
    let (dx,dy) = p
    
    vertex (Vertex3 (fromIntegral w  * floatToGLfloat dx) (fromIntegral h * floatToGLfloat dy) (1::GL.GLfloat))
    
    placeColorVerticies w h ps



-- Function to create a new source texture when a board recursively draws onto itself using a FBO
-- This prevents a feedback loop with undefined behavior that would draw and read from the same texture at the same time
--   * texInfoS - the texture information for the texture that is supposed to be drawn onto itself
--   returns - the new texture id of the copied texture and the pointer to this new texture (used to delete it)
fixTexLoopback :: TextureInfo -> IO ( (GLuint, Ptr GLuint) )
fixTexLoopback texInfoS = do
    let (w,h) = texSize texInfoS
        colorType = texFormat texInfoS
    
    -- Create and bind a new texture object to use as the source texture for splating
    texIdPtr <- malloc :: IO(Ptr GLuint)
    glGenTextures 1 texIdPtr
    texId <- peek texIdPtr
    glBindTexture gl_TEXTURE_2D texId
    
    --Set up the texture object and its parameters
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR
    --glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR_MIPMAP_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE

    -- Copy the texture from the current FBO
    glCopyTexImage2D gl_TEXTURE_2D 0 (fromIntegral colorType) 0 0 (fromIntegral w) (fromIntegral h) 0
    -- Unbind the new texture
    glBindTexture gl_TEXTURE_2D 0
    
    return (texId, texIdPtr)






-- Function to splat a color polygon onto one destination buffer
--   * (r,g,b,a) - The color to splat onto the destination board
--   * bD - The destination buffer (board) object name
--   * ps - A list of UIPoints, which specify the points of the colored polygon to draw onto the destination buffer
splatColor :: RGBA -> BufferId -> Bool -> [UIPoint] -> CBM ()
splatColor (T.RGBA r g b a) bD _useBlend ps = do
    fboSupp <- getFBOSupport
    texMap <- getTexMap

    liftIO $ do
            -- Check to make sure the destination board exists
            when (notMember bD texMap) $ do
                    print "Error: The destination board to splat to doesn't exist."
                    exitWith (ExitFailure 1)
            
            -- Look up all of the values that will be needed
            let (Just texInfoD) = lookup bD texMap
                texIdPtrD = texPtr texInfoD
                (w,h) = texSize texInfoD
                colorType = texFormat texInfoD
                
            texIdD <- peek texIdPtrD

            if (not fboSupp)
                then do
                    clear [DepthBuffer] -- clear the depth buffer
                    loadIdentity
                    --Bind the destination texture to use first
                    glBindTexture gl_TEXTURE_2D texIdD
                    -- Turn off blending so the destination board isn't blended with the background color
                    blend $= Disabled
                    -- Render the destination board so we can draw onto it
                    renderPrimitive Quads $ do
                        texCoord (TexCoord2 0 (0::GL.GLfloat)) -- Bottom Left
                        vertex (Vertex3 0 0 (0::GL.GLfloat)) -- Used to be GL.GLsizei, does it matter?
                        texCoord (TexCoord2 1 (0::GL.GLfloat)) -- Bottom Right
                        vertex (Vertex3 (fromIntegral w) 0 (0::GL.GLfloat))
                        texCoord (TexCoord2 1 (1::GL.GLfloat)) -- Top Right
                        vertex (Vertex3 (fromIntegral w) (fromIntegral h) (0::GL.GLfloat))
                        texCoord (TexCoord2 0 (1::GL.GLfloat)) -- Top Left
                        vertex (Vertex3 0 (fromIntegral h) (0::GL.GLfloat))
                    -- Turn blending back on so that the source board can be blended with the destination board
                    blend $= Enabled
                    -- Unbind the texture to prevent mapping - doesn't work if this is removed.
                    glBindTexture gl_TEXTURE_2D 0
                else do
                    -- Attach the texture to a FBO color attachment point
                    -- AJG: This is also taking a lot of time, presumbily because it stalls the pipeline.
                    -- We can store what we've alread done, and not redo this for each target.
		    bindFrameBufferToTexture texIdD (w,h)
                    -- glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D texIdD 0
            
            -- Switch the color to the one we are trying to splat
            color (Color4 (floatToGLclampf r) (floatToGLclampf g) (floatToGLclampf b) ((floatToGLclampf a)::GLclampf))
            -- Uses relative positions (percentages) of source and destination boards
            -- most of the time is attributed to *renderPrimitive* (glBegin?)

            renderPrimitive Polygon $ do
                placeColorVerticies w h ps

            when (not fboSupp) $ do
                -- Bind the destination texture so we can copy the new image out to it
                glBindTexture gl_TEXTURE_2D texIdD
                -- Copy the texture from the framebuffer
                glCopyTexImage2D gl_TEXTURE_2D 0 (fromIntegral colorType) 0 0 (fromIntegral w) (fromIntegral h) 0 --make more efficient by only copying the changed subimage?
                -- Unbind Texture until it is needed (may want to take this out depending on how we order instructions coming in)
                glBindTexture gl_TEXTURE_2D 0






-- Function to copy the image from one buffer into another buffer (using either its original alpha or the destination buffer's alpha)
--   * alpha - WithSrcAlpha to do a normal copy, keeping its own alpha values. WithDestAlpha to use the destination buffer's alpha values
--   * bS - The source buffer id
--   * bD - The destination buffer id
copyBuffer :: WithAlpha -> BufferId -> BufferId -> CBM ()
copyBuffer alpha bS bD = do
    case alpha of
            WithSrcAlpha -> do
                    liftIO $ blendFuncSeparate $= ((One, Zero), (One, Zero))
                    splatPolygon bS bD [ PointMap p p | p <- [(0,0),(0,1),(1,1),(1,0)] ]
                    liftIO $ blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, OneMinusSrcAlpha))
            WithDestAlpha -> do
                    liftIO $ blendFuncSeparate $= ((One, Zero), (Zero, One))
                    splatPolygon bS bD [ PointMap p p | p <- [(0,0),(0,1),(1,1),(1,0)] ]
                    liftIO $ blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, OneMinusSrcAlpha))






-- Function to save an image out to a file from a specified board/buffer
--   * b        - The name of the buffer (board) object that will be saved out to a file
--   * savePath - The file path to the location where the image should be saved (including the image filename and extension)
-- This could maybe save a lot of code by just calling splatPolygon, but there may be complications.
saveImage :: BufferId -> FilePath -> CBM ()
saveImage b savePath = do
    fboSupp <- getFBOSupport
    texMap <- getTexMap
    
    liftIO $ do
            -- Check to make sure the board being saved exists
            when (notMember b texMap) $ do
                    print "Error: The board to be saved doesn't exist."
                    exitWith (ExitFailure 1)
            
            -- Check if an image with the same name already exists. If it does, delete it.
            alreadyExists <- doesFileExist $ savePath
            when (alreadyExists) $ removeFile savePath
            
            -- Look up the board we need to save
            let (Just texInfo) = lookup b texMap
                texIdPtr = texPtr texInfo
                (w,h) = texSize texInfo
            
            texId <- peek texIdPtr

            -- Create and bind a new texture object to use as the RGBA texture for outputting
            texIdPtr2 <- malloc :: IO(Ptr GLuint)
            glGenTextures 1 texIdPtr2
            texId2 <- peek texIdPtr2
            glBindTexture gl_TEXTURE_2D texId2
            
            --Set up the texture object and its parameters
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR
            --glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR_MIPMAP_LINEAR
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
            glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
            
            when (fboSupp) $ do
                    -- Create the new texture object so that we can draw directly into it
                    glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGBA) (fromIntegral w) (fromIntegral h) 0 gl_RGBA gl_UNSIGNED_BYTE nullPtr
                    -- Attach the new texture to a FBO color attachment point
		    bindFrameBufferToTexture texId2 (w,h)
                    -- glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D texId2 0
                    
            -- Bind the original (non-RGBA) texture so that it can be copied into the new one
            glBindTexture gl_TEXTURE_2D texId
            
            clear [ColorBuffer, DepthBuffer] -- clear the screen and the depth buffer
            loadIdentity -- reset view
            
            color (Color4 1 1 1 (1::GL.GLfloat))
            -- Render the final board we want to save to file
            renderPrimitive Quads $ do
                texCoord (TexCoord2 0 (0::GL.GLfloat)) -- Bottom Left
                vertex (Vertex3 0 0 (0::GL.GLfloat)) -- Used to be GL.GLsizei, does it matter?
                texCoord (TexCoord2 1 (0::GL.GLfloat)) -- Bottom Right
                vertex (Vertex3 (fromIntegral w) 0 (0::GL.GLfloat))
                texCoord (TexCoord2 1 (1::GL.GLfloat)) -- Top Right
                vertex (Vertex3 (fromIntegral w) (fromIntegral h) (0::GL.GLfloat))
                texCoord (TexCoord2 0 (1::GL.GLfloat)) -- Top Left
                vertex (Vertex3 0 (fromIntegral h) (0::GL.GLfloat))
            
            -- Bind the new texture again so that it can be saved
            glBindTexture gl_TEXTURE_2D texId2
            
            if (fboSupp)
                then do
                    -- Unattach the new texture from the FBO color attachment point since it will be deleted
		    bindFrameBufferToTexture 0 (1,1)	--- wrong size; does not matter?
                    -- glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D 0 0
                else do
                    -- Copy the texture from the screen to the new texture for saving
                    glCopyTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGBA) 0 0 (fromIntegral w) (fromIntegral h) 0   


            -- Create a new array with the image data so that we can write it out with devIL
            let arrBounds = ((0,0,0), (fromIntegral h-1, fromIntegral w-1, 3))
            arr <- (newArray_ arrBounds) :: IO (StorableArray (Int,Int,Int) Word8)
            
            -- Have OpenGL fill the array with the texture data and then write out that data to an image file using DevIL
            withStorableArray arr $ \ptr2 -> do
                glGetTexImage gl_TEXTURE_2D 0 gl_RGBA gl_UNSIGNED_BYTE (castPtr ptr2)
                writeImageFromPtr savePath (fromIntegral h, fromIntegral w) (castPtr ptr2)
            
            -- Unbind this texture so it isn't the one being used anymore
            glBindTexture gl_TEXTURE_2D 0
            
            -- Delete the texture since it won't be needed anymore
            glDeleteTextures 1 texIdPtr2
            free texIdPtr2






-- Function to delete a specified board/buffer from memory
--   * b - The name of the buffer (board) object to be deleted
deleteBuffer :: BufferId -> CBM ()
deleteBuffer b = do
    texMap <- getTexMap
    
    liftIO $ do
            -- Check to make sure the board being deleted exists
            when (notMember b texMap) $ do
                    print "Error: The board to be saved doesn't exist."
                    exitWith (ExitFailure 1)
            
            -- Look up the board we need to delete
            let (Just texInfo) = lookup b texMap
                texIdPtr = texPtr texInfo
            
            -- Delete the texture
            glDeleteTextures 1 texIdPtr
            free texIdPtr
    
    -- This deletes the mapping
    setTexMap (delete b texMap)










floatToGLfloat :: Float -> GL.GLfloat
floatToGLfloat = realToFrac

floatToGLclampf :: Float -> GL.GLclampf
floatToGLclampf = realToFrac
 

-- Should be in the CB monad, and lookup the size in a table.
bindFrameBufferToTexture :: (Integral a) => GLuint -> (a,a) -> IO ()
bindFrameBufferToTexture tex (x,y) = do
	    glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D tex 0
	    viewport   $= (Position 0 0, Size (fromIntegral x) (fromIntegral y)) -- (fromIntegral w) (fromIntegral h))

            matrixMode $= Projection
            loadIdentity
            ortho2D 0 (fromIntegral x) 0 (fromIntegral y) -- Will probably want to change this from using the window w/h
            matrixMode $= Modelview 0
--	    flush

	
{-
    -- Draw a circle (or an ellipse by scaling the circle)
    -- Parameters would be xPos, yPos, rotationDegree, xScale, yScale, Color4(RGBA), radius, slices (# of points used to draw it)
    translate (Vector3 100 100 (0::GL.GLfloat)) -- Move it around to center on right position
    rotate 15 (Vector3 0 0 (-1::GL.GLfloat)) -- Rotate about -z axis so that positive degrees are clockwise
    scale 1 0.5 (1::GL.GLfloat) -- Scale the circle if it should be more of an ellipse
    color (Color4 1 0 0 (0.3::GL.GLfloat)) -- Change the color (and alpha)
    renderQuadric (QuadricStyle (Just Smooth) NoTextureCoordinates Inside FillStyle) (Disk 0 100 100 1) --inner radius, outer radius, slices, loops
--}







