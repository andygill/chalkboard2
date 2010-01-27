module Main where

import Graphics.ChalkBoard as CB
import Data.IORef


main = startChalkBoard [BoardSize 400 400] $ \ cb -> do

        switch <- newIORef True
        mouseCallback cb (mouseCB switch)
        
        pause <- newIORef False
        keyboardCallback cb (keyboardCB pause)
        
	let loop x = do
	        pause' <- readIORef pause
                switch' <- readIORef switch
                if switch'
                        then drawChalkBoard cb (brd1 x)
                        else drawChalkBoard cb (brd2 x)
                if pause'
                        then loop x
                        else loop (x+1)

	loop 0

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
mouseCB switch (x,y) = do
        if (x < 0.2 && x > (-0.2) && y < 0.2 && y > (-0.2))
                then modifyIORef switch not
                else return ()

keyboardCB :: IORef(Bool) -> Char -> IO ()
keyboardCB pause 'p' = do
        modifyIORef pause not
keyboardCB _ _ = return ()


