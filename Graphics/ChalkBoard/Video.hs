module Graphics.ChalkBoard.Video where


import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.Buffer
import Graphics.ChalkBoard.IStorable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

import System.Process
import System.IO
import Foreign.Ptr ( Ptr )
import Data.Word ( Word8 )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, takeMVar, putMVar )


newtype InPipe = InPipe Handle
newtype OutPipe = OutPipe Handle


openVideoInPipe :: String -> IO (InPipe)
openVideoInPipe ffmpegCmd = do
    (_, Just hout, _, _) <- createProcess (shell ffmpegCmd){ std_out = CreatePipe, close_fds = True }
    return (InPipe hout)


nextPPMFrame :: InPipe -> IO (Bool, Buffer RGB)
nextPPMFrame (InPipe h) = do
    ty <- hGetLine h
    bin <- case ty of
        "P6" -> return True
        _ -> error $ "bad PPM format: " ++ ty
    szs <- hGetLine h
    let [width,height] = (map read (words szs) :: [Int])
    --print width
    --print height   
    mx <- hGetLine h
    let mxs = (map read (words mx) :: [Int]) -- TODO: Check that maxs = 255?
    --print (head mxs)

    checkEOF <- newEmptyMVar    
    bs <- BSI.create (width * height * 3) (fn h width height (head mxs) checkEOF)
    eof <- takeMVar checkEOF

    return (eof, newBufferRGB bs (width,height)) -- TODO: Determine if the pipe has been closed and return True/False based on that
    
    where fn hIn w h mx eof ptr = do
            bytesRead <- hGetBuf hIn ptr (w*h*3) -- TODO: Use bytes read to determine if the pipe is closed
            if (bytesRead == w*h*3)
                then putMVar eof True
                else putMVar eof False
            return ()



openVideoOutPipe :: String -> IO (OutPipe)
openVideoOutPipe ffmpegCmd = do
    (Just hin, _, _, _) <- createProcess (shell ffmpegCmd){ std_in = CreatePipe, close_fds = True }
    return (OutPipe hin)


writeNextFrame :: OutPipe -> (Int, Int) -> Ptr Word8 -> IO ()
writeNextFrame (OutPipe hout) (w,h) buffer = do
    hPutStrLn hout "P6"
    hPutStrLn hout (show w ++ " " ++ show h)
    hPutStrLn hout "255"
    hPutBuf hout buffer (w*h*3)
    hPutStrLn hout ""

closeVideoOutPipe :: OutPipe -> IO ()
closeVideoOutPipe (OutPipe hout) = do
    hClose hout


ffmpegOutCmd :: String -> String
ffmpegOutCmd filename = "ffmpeg -f image2pipe -vcodec ppm -i - -f h264 -b 500k " ++ filename

ffmpegInCmd :: String -> String
ffmpegInCmd filename = "ffmpeg -i " ++ filename ++ " -f image2pipe -vcodec ppm -"




