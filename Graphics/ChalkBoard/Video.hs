module Graphics.ChalkBoard.Video where


import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.Buffer
import Graphics.ChalkBoard.IStorable

import System.Process
import System.IO


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
    print width
    print height   
    mx <- hGetLine h
    let mxs = (map read (words mx) :: [Int]) -- TODO: Check that maxs = 255?
    print (head mxs)

    arr <- newReadOnlyCByteArray (width*height*3) (fn h width height (head mxs))
	
    return (True, newBufferRGB arr (width,height)) -- TODO: Determine if the pipe has been closed and return True/False based on that
    
    where fn hIn w h mx ptr = do
            bytesRead <- hGetBuf hIn ptr (w*h*3) -- TODO: Use bytes read to determine if the pipe is closed
            return ()

