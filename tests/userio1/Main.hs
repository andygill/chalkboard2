module Main where

import Graphics.ChalkBoard as CB
import Data.IORef


main = startChalkBoard [BoardSize 400 400] $ \ cb -> do

        switch <- newIORef True
        mouseCallback cb (mouseCB switch)
        
	let loop ref x = do
                bool <- readIORef ref
                if bool
                        then drawChalkBoard cb (brd1 x)
                        else drawChalkBoard cb (brd2 x)
                loop ref (x+1)

	loop switch 0

	exitChalkBoard cb


brd1 :: Int -> Board RGB
brd1 a = {-unAlphaBoard (boardOf white)-} (move (ui/2,ui/2) (rotate (4*(fromIntegral a)) (scale ui $ choose red green <$> circle)))
        where ui = sin ((fromIntegral a)/100)
              (x,y) = (fromIntegral (a `mod` 100) / 99.0, fromIntegral (a `mod` 100) / 99.0)

brd2 :: Int -> Board RGB
brd2 a = {-unAlphaBoard (boardOf white)-} (move (ui/2,-ui/2) (rotate (4*(fromIntegral a)) (scale ui $ choose red green <$> circle)))
        where ui = sin ((fromIntegral a)/100)
              (x,y) = (fromIntegral (a `mod` 100) / 99.0, fromIntegral (a `mod` 100) / 99.0)



mouseCB :: IORef(Bool) -> (Float,Float) -> IO ()
mouseCB switch _ = do
        modifyIORef switch not

