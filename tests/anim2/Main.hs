module Main where

import Graphics.ChalkBoard as CB
import qualified Control.Applicative as A
import Control.Concurrent as C
import Data.Boolean

main = startChalkBoard [BoardSize 400 400] $ \ cb -> do

	rt <- realTime			-- plays in real time
--	rt <- byFrame 29.97		-- plays in 'frame' time

	let loop = do
		optBrd <- playActive rt anim
		case optBrd of
		   Just brd -> do
			drawChalkBoard cb brd
			loop
		   Nothing -> return ()

--	startMyWriteStream cb "ffmpeg -f image2pipe -vcodec ppm -i - -vcodec libx264 -b 500k -vpre hq -vpre main orbit.mp4" 
	loop
--	endWriteStream cb

	exitChalkBoard cb

anim :: Active (Board RGB)
anim = fmap (CB.maybe white id .$) $
	foldr1 page
	      [  -- empty nothing with zero time (just a filler, to make commenting things out easy)
		for 0 $ A.pure (boardOf (just white))
		  -- show earth
	      , earth
		  -- add orbit path
	      , for 3 (color red (intro 2 (satPath o1))) `over` (snap 1 earth)
		  -- add orbiting satillite
	      , color green (intro 2 (satMove 5 o1)) `over` (color red (satPath o1)) `over` (snap 1 earth)
		  -- add target path
	      , color red (intro 2 (satPath o3))
			`over` color green (satMove 5 o1)
			`over` color red (satPath o1) 
			`over` (snap 1 earth)
		  -- move sat
	      , color green (satMove 5 o2) `over` (color red (satPath o2)) `over` (color red (satPath o3)) `over` (snap 1 earth)
	      , color green (satMove 20 o3) `over` (color red (satPath o3)) `over` (snap 1 earth)
	      ]

earth :: Active (Board (Maybe RGB))
earth = color blue
      $ fmap (\ ui -> scale (0.2) $ (choose (o ui) (o 0) <$> circle)) 
      $ scale 1
      $ age

o1 = \ x -> ((cos (x*2*pi))*0.2,(sin (x*2*pi))*0.2)
o2 = \ y -> let x = y / 2 in ((cos (x*2*pi))*0.3-0.1,(sin (x*2*pi))*0.2) 
o3 = \ x -> (-(cos (x*2*pi))*0.4,-(sin (x*2*pi))*0.4)

color :: O RGB -> Active (Board UI) -> Active (Board (Maybe RGB))
color rgb = fmap (mapMaybe (mix white rgb) .$) . fmap (trans 0 .$)

satPath :: (UI -> Point) ->  Active (Board UI)
satPath fn =  A.pure (choose 1 0 <$> functionLine fn 0.005 100)

satMove :: R -> (UI -> Point) -> Active (Board UI)
satMove n fn =
      ( fmap (\ x -> move (fn x) 
	           $ scale (0.1) 
	           $ (choose 1 0 <$> circle))
	  (scale n $ age)
      )
                
intro :: Float -> Active (Board UI) -> Active (Board UI)
intro n a = (\ b x -> mix 0 (o x) <$> b) A.<$> a A.<*> scale n age

exit :: Float -> Active (Board UI) -> Active (Board UI)
exit n = CB.reverse . intro n . CB.reverse

trans :: (Maskable a, Eq a) => O a -> O a -> O (Maybe a)
trans z a = withMask a (a /=* z)