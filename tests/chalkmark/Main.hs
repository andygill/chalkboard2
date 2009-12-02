{-# OPTIONS_GHC -ddump-simpl-stats #-}
module Main where
	
import Graphics.ChalkBoard as CB hiding (Board)
--import Control.Applicative
import Data.Array
import System.Cmd
import Graphics.ChalkBoard


main = startChalkBoard [] $ \ chalkBoard -> do
	let loop n d | n > 3 = exitChalkBoard chalkBoard
      	    loop n d = do
  	        drawChalkBoard chalkBoard (example10 n)
	        loop (n+d) d
	loop 0 0.01

example10 :: R -> Board RGB
example10 rX = rotate rX $ scale (0.5 + 0.5 * log (1 + (rX / 1))) 
	$ (unAlpha <$> (foldr (\ (x,y) w -> cir1 (x,y) (rX/(x+count)) (rX/(y+count)) `over` w) (boardOf (alpha white))
		$ [ (x,y) | x <- [0..count], y <- [0..count]]))
  where
    cir1 (x,y) r r2 =  
	   move (-0.5 + x / count,-0.5 + y / count)
         $ rotate (r2 * 200)  	
	 $ scaleXY (0.5,1)
  	 $ scale (0.8 / count)
	 ( choose (alpha c) (transparent white) <$> square)
       where c = o $ RGB (x/count) (y/count) ((rX / 3) - fromIntegral (floor (rX / 3)))
	     r' = r * 50 + y
	
    count = 20 -- + (fromIntegral(round (rX * 2) :: Int))
--	20 -- 10 -- 1-- 20


