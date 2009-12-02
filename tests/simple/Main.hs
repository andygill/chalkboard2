module Main where

import Graphics.ChalkBoard


main = startChalkBoard [] cbMain



cbMain cb = do
        let example1 = boardOf blue
            example2 = scale 0.5 $ choose red green <$> square
            example3 = scale 0.5 $ choose red blue <$> circle `over` (scale 0.9 square)
            example4 = unAlpha <$> scale 0.5 (choose (withAlpha 0.2 blue) (transparent white) <$> triangle (-0.5,-0.5) (0.5,-0.5) (0,0.5))
            example5 = unAlpha <$> ((scale 0.7 cir) `over` poly)
                    where
                            cir = choose (withAlpha 0.5 blue) (transparent white) <$> circle
                            poly = choose (alpha red) (transparent white) <$> polygon [(0,-0.5), (-0.4,-0.3), (0,0.5), (0.4,-0.3)]
        
        
        (w,h,imgBrd) <- readBoard ("lambda.png")
        let wh = fromIntegral $ max w h
            sc = 1 / wh
            wd = fromIntegral w / wh
            hd = fromIntegral h / wh
            img = move (-0.5 * hd,-0.5 * wd)  (scale sc imgBrd)
	    
	(w2,h2,img2) <- readNormalizedBoard ("lambda.png")
	    
	let example6 = unAlpha <$> img2
	    example7 = unAlpha <$> img2 `over` (boardOf (alpha red))
	    example8 = unAlpha <$> (rotate 1 (scale 0.8 img2)) `over` (boardOf (alpha red))
	    
	    
	let example9 x = unAlpha <$> (rotate x (scale 0.8 img2)) `over` (boardOf (alpha red))
	    example10 x = unAlpha <$> (rotate (4*x) (scale x img2)) `over` (boardOf (alpha (o (RGB x x x))))
	
        
        drawChalkBoard cb example6
        --sequence_ [ drawChalkBoard cb (example9 x) | x <- [0,0.01..] ]






{-
example1 cb = drawChalkBoard cb $ boardOf blue

example2 cb = drawChalkBoard cb $ scale 0.5 $ choose red blue <$> square

--}
