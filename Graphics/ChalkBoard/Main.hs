module Graphics.ChalkBoard.Main 
	( ChalkBoard
	, drawChalkBoard
	, drawChalkBuffer
	, writeChalkBoard
	, startMyWriteStream
	, frameChalkBoard
	, startDefaultWriteStream
	, endWriteStream
	, updateChalkBoard
	, mouseCallback
	, keyboardCallback
	, drawRawChalkBoard
	, exitChalkBoard
	, startChalkBoard
	, openChalkBoard
	, chalkBoardServer
	) where

import System.Process
import System.Environment
import System.IO
import System.Exit
import Control.Concurrent 

import Graphics.ChalkBoard.Core
import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.Board
import Graphics.ChalkBoard.Buffer
import Graphics.ChalkBoard.CBIR
import Graphics.ChalkBoard.CBIR.Compiler
import Graphics.ChalkBoard.OpenGL.CBBE
import Graphics.ChalkBoard.O
import Graphics.ChalkBoard.Options
import Graphics.ChalkBoard.Video ( ffmpegOutCmd )
import Codec.Image.DevIL

import Data.Word
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad ( when )
import System.Cmd
import Data.Binary as Bin

import qualified Data.ByteString.Lazy as B
	
	
data ChalkBoardCommand
	= DrawChalkBoard (Board RGB)
	| DrawChalkBuffer (Buffer RGB)
	| UpdateChalkBoard (Board RGB -> Board RGB)
	| WriteChalkBoard FilePath
	| StartMyWriteStream String (MVar StreamId)
	| StartDefaultWriteStream FilePath (MVar StreamId)
	| FrameChalkBoard StreamId
	| EndWriteStream StreamId
	| NewMouseCallback (UIPoint -> IO())
	| NewKeyboardCallback (Char -> IO())
	| ExitChalkBoard
	| DrawRawChalkBoard [Inst BufferId]
	
{-
	| OpenStream String (MVar Int)

openStream (.. var) str = do
	ref <- newEmptyMVar
	putMVar var (OpenStream str ref)
	ans <- takeMVar ref
	return $ HOut ans
-}

data ChalkBoard = ChalkBoard (MVar ChalkBoardCommand) (MVar ())

data StreamHandle = StreamHandle StreamId

-- | Draw a board onto the ChalkBoard.
drawChalkBoard :: ChalkBoard -> Board RGB -> IO ()
drawChalkBoard (ChalkBoard var _) brd = putMVar var (DrawChalkBoard brd)

-- | Draw a chalkbuffer onto the ChalkBoard.
-- Internally, this is stored in the buffer size,
-- regardless of the size of the viewers screen.
-- updateChalkBoard does not work after drawChalkBoard.
drawChalkBuffer :: ChalkBoard -> Buffer RGB -> IO ()
drawChalkBuffer (ChalkBoard var _) buff = putMVar var (DrawChalkBuffer buff)

-- | Write the contents of a ChalkBoard into a File.
writeChalkBoard :: ChalkBoard -> FilePath -> IO ()
writeChalkBoard (ChalkBoard var _) nm = putMVar var (WriteChalkBoard nm)

-- | Start streaming the contents of a ChalkBoard buffer into a File.
startMyWriteStream :: ChalkBoard -> String -> IO StreamHandle
startMyWriteStream (ChalkBoard var _) cmd = do
		res <- newEmptyMVar
		putMVar var (StartMyWriteStream cmd res)
		sid <- takeMVar res
		return $ StreamHandle sid

-- | Start streaming the contents of a ChalkBoard buffer into a File, with given ffmpeg command
startDefaultWriteStream :: ChalkBoard -> FilePath -> IO StreamHandle
startDefaultWriteStream (ChalkBoard var _) nm = do
		res <- newEmptyMVar
		putMVar var (StartDefaultWriteStream nm res)
		sid <- takeMVar res
		return $ StreamHandle sid

--doWriteFrame :: ChalkBoard -> 

-- | End streaming the contents of a ChalkBoard buffer into a File.
endWriteStream :: ChalkBoard -> StreamHandle -> IO ()
endWriteStream (ChalkBoard var _) (StreamHandle sid) = putMVar var (EndWriteStream sid)

-- | modify the current ChalkBoard.
updateChalkBoard :: ChalkBoard -> (Board RGB -> Board RGB) -> IO ()
updateChalkBoard (ChalkBoard var _) brd = putMVar var (UpdateChalkBoard brd)

frameChalkBoard :: ChalkBoard -> StreamHandle -> IO ()
frameChalkBoard (ChalkBoard var _) (StreamHandle sid) = putMVar var (FrameChalkBoard sid)


mouseCallback :: ChalkBoard -> (UIPoint -> IO ()) -> IO ()
mouseCallback (ChalkBoard var _) fn = putMVar var (NewMouseCallback fn)

keyboardCallback :: ChalkBoard -> (Char -> IO()) -> IO ()
keyboardCallback (ChalkBoard var _) fn = putMVar var (NewKeyboardCallback fn)


-- | Debugging hook for writing raw CBIR code.
drawRawChalkBoard :: ChalkBoard -> [Inst BufferId] -> IO ()
drawRawChalkBoard (ChalkBoard var _) cmds = putMVar var (DrawRawChalkBoard cmds)

-- | pause for this many seconds, since the last redraw *started*.
pauseChalkBoard :: ChalkBoard -> Double -> IO ()
pauseChalkBoard _ n = do
	threadDelay (fromInteger (floor (n * 1000000)))
	
-- | quit ChalkBoard.
exitChalkBoard :: ChalkBoard -> IO ()
exitChalkBoard (ChalkBoard var end) = do
	putMVar var ExitChalkBoard
	takeMVar end 
	return ()

-- | Start, in this process, a ChalkBoard window, and run some commands on it.
startChalkBoard :: [Options] -> (ChalkBoard -> IO ()) -> IO ()
startChalkBoard options cont = do
	putStrLn  "[Starting ChalkBoard]"
	ilInit

	v0 <- newEmptyMVar
	v1 <- newEmptyMVar 
	v2 <- newEmptyMVar 
	vEnd <- newEmptyMVar
	
	forkIO $ compiler options v1 v2 
	forkIO $ do
		() <- takeMVar v0
		cont (ChalkBoard v1 vEnd)
		print "[Done]"
	startRendering viewBoard v0 v2 options
	
	return ()	

-- | Open, remotely, a ChalkBoard windown, and return a handle to it.
-- Needs "CHALKBOARD_SERVER" set to the location of the ChalkBoard server.
openChalkBoard :: [Options] -> IO ChalkBoard
openChalkBoard args = do
	putStrLn "[Opening Channel to ChalkBoard Server]"
	ilInit

	v0 <- newEmptyMVar
	v1 <- newEmptyMVar 
	v2 <- newEmptyMVar 
	vEnd <- newEmptyMVar

	(ein,eout,err,pid) <- openServerStream		
	
	let options = encode (args :: [Options])
	B.hPut ein (encode (fromIntegral (B.length options) :: Word32))
	B.hPut ein options
	hFlush ein

	forkIO $ compiler args v1 v2
	forkIO $ do
		let loop n = do
			v <- takeMVar v2
			let code = encode v
			B.hPut ein (encode (fromIntegral (B.length code) :: Word32))
			B.hPut ein code
			hFlush ein
			case v of
			  [Exit] -> putMVar vEnd ()
			  _ -> loop $! (n+1)
		loop 0

	return (ChalkBoard v1 vEnd)

viewBoard :: Int
viewBoard = 0


compiler :: [Options] -> MVar ChalkBoardCommand -> MVar [Inst Int] -> IO ()
compiler options v1 v2 = do
	putMVar v2 [Allocate viewBoard (x,y) RGB24Depth (BackgroundRGB24Depth (RGB 1 1 1))]
	loop (0::Integer) (boardOf (o (RGB 1 1 1))) (0 :: BufferId)
  where     
     (x,y) = head ([ (x,y) | BoardSize x y <- options ] ++ [(400,400)])
     loop n old_brd buffIds = do
	cmd <- takeMVar v1
	case cmd of
	  DrawChalkBoard brd -> do
		cmds <- compile (x,y) viewBoard (move (0.5,0.5) brd)
		when (elem DebugCBIR options) $ do
			putStrLn $ showCBIRs cmds
		putMVar v2 cmds
		loop (n+1) brd buffIds
	  DrawChalkBuffer buff -> do
		cmds <- compileB (x,y) viewBoard buff
		when (elem DebugCBIR options) $ do
			putStrLn $ showCBIRs cmds
--		putStrLn $ showCBIRs cmds
		putMVar v2 cmds
		loop (n+1) (error "no ChalkBoard") buffIds
	  UpdateChalkBoard fn -> do
		let brd = fn old_brd
		cmds <- compile (x,y) viewBoard (move (0.5,0.5) brd)
--		putStrLn $ showCBIRs cmds
		putMVar v2 cmds
		loop (n+1) brd buffIds
	  DrawRawChalkBoard cmds -> do
		putMVar v2 cmds
		loop (n+1) (error "Board in an unknown state") buffIds
	  WriteChalkBoard filename -> do
		putMVar v2 [SaveImage viewBoard filename]
		loop (n+1) old_brd buffIds
	  StartMyWriteStream openCmd v0 -> do
	        putMVar v2 [OpenStream buffIds openCmd (elem VerboseVideo options)]
		putMVar v0 buffIds
	        loop (n+1) old_brd (succ buffIds)
	  StartDefaultWriteStream filename v0 -> do
	        putMVar v2 [OpenStream buffIds (ffmpegOutCmd filename) (elem VerboseVideo options)]
		putMVar v0 buffIds
	        loop (n+1) old_brd (succ buffIds)
	  FrameChalkBoard sid -> do
	        putMVar v2 [WriteStream viewBoard sid]
	        loop (n+1) old_brd buffIds
	  NewMouseCallback fn -> do
	        putMVar v2 [ChangeMouseCallback fn]
	        loop (n+1) old_brd buffIds
	  NewKeyboardCallback fn -> do
	        putMVar v2 [ChangeKeyboardCallback fn]
	        loop (n+1) old_brd buffIds
	  EndWriteStream sid -> do
	        putMVar v2 [CloseStream sid]
	        loop (n+1) old_brd buffIds

	  ExitChalkBoard -> putMVar v2 [Exit]


openServerStream :: IO (Handle,Handle,Handle,ProcessHandle)
openServerStream = do
	server <- getEnv "CHALKBOARD_SERVER" `catch` (\ _ -> return "chalkboard-server-1_9_0_19")
	runInteractiveProcess server [] Nothing Nothing `catch` (\ _ -> do print "DOOL" ; error "")


-- | create an instance of the ChalkBoard. Only used by the server binary.
chalkBoardServer :: IO ()
chalkBoardServer = do
	v0 <- newEmptyMVar
	v2 <- newEmptyMVar 
	bs <- B.hGet stdin 4
	let n :: Word32
	    n = Bin.decode bs
	options <- B.hGet stdin (fromIntegral n)
	forkIO $ do
		let loop = do
			bs <- B.hGet stdin 4
			let n :: Word32
			    n = Bin.decode bs
			packet <- B.hGet stdin (fromIntegral n)
			putMVar v2 (decode packet :: [Inst BufferId])
			loop
		loop
	startRendering viewBoard v0 v2 (decode options :: [Options])
	return ()	



	
