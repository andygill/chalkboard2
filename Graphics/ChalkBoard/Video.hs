module Graphics.ChalkBoard.Video where


import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.Buffer
import qualified Data.ByteString.Internal as BSI

import System.Process
import System.IO
import Foreign.Ptr ( Ptr )
import Data.Word ( Word8 )



newtype InPipe = InPipe Handle
newtype OutPipe = OutPipe Handle



openVideoInPipe :: String -> IO (InPipe)
openVideoInPipe ffmpegCmd = do
    (Just hin, Just hout, Just herr, _) <- createProcess (shell ffmpegCmd){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, close_fds = True }
    --(Just hin, Just hout, _, _) <- createProcess (shell ffmpegCmd){ std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit, close_fds = True }
    hClose hin
    hClose herr
    return (InPipe hout)

{- It would be nice to do this... but doesn't seem to work.
openVideoWithInPipe :: String -> InPipe -> IO ()
openVideoWithInPipe ffmpegCmd (InPipe h) = do
    (Just hin, Nothing, Just herr, _) <- createProcess (shell ffmpegCmd){ std_in = CreatePipe, std_out = UseHandle h, std_err = CreatePipe, close_fds = True }
    hClose hin
    hClose herr
-}

nextPPMFrame :: InPipe -> IO (Maybe (Buffer RGB))
nextPPMFrame (InPipe hIn) = do
    eof <- hIsEOF hIn
    if (eof)
        then return Nothing
        else do
            ty <- hGetLine hIn
            _ <- case ty of
                "P6" -> return True
                _ -> error $ "bad PPM format: " ++ ty
            szs <- hGetLine hIn
            let [width,height] = (map read (words szs) :: [Int])
            --print width
            --print height   
            mx <- hGetLine hIn
            let mxs = (map read (words mx) :: [Int]) -- TODO: Get the max easier somehow?
            --print (head mxs)
        
            bs <- BSI.create (width * height * 3) (fn hIn width height (head mxs))
            
            return (Just (newBufferRGB bs (width,height)))
            
            where fn handle w h _ ptr = do
                    _ <- hGetBuf handle ptr (w*h*3)
                    return ()

closeVideoInPipe :: InPipe -> IO ()
closeVideoInPipe (InPipe hin) = do
    hClose hin

openVideoOutPipe :: Bool -> String -> IO (OutPipe)
openVideoOutPipe True ffmpegCmd = do
    (Just hin, _, _, _) <- createProcess (shell ffmpegCmd){ std_in = CreatePipe, close_fds = True }
    return (OutPipe hin)
openVideoOutPipe False ffmpegCmd = do
    (Just hin, Just hout, Just herr, _) <- createProcess (shell ffmpegCmd){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, close_fds = True }
    --(Just hin, Just hout, _, _) <- createProcess (shell ffmpegCmd){ std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit, close_fds = True }
    hClose hout
    hClose herr
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
ffmpegOutCmd filename = "ffmpeg -f image2pipe -vcodec ppm -i - -vcodec libx264 -b 500k -vpre hq -vpre main " ++ filename

ffmpegInCmd :: String -> String
ffmpegInCmd filename = "ffmpeg -i " ++ filename ++ " -f image2pipe -vcodec ppm -"




