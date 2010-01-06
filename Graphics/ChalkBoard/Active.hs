module Graphics.ChalkBoard.Active where
	
import Control.Applicative
import Graphics.ChalkBoard.Types 
import Graphics.ChalkBoard.Utils
import Graphics.ChalkBoard hiding (zip)
import Data.Ratio
import Prelude hiding (zip)

data Active a 
	= Active Rational		-- start
	         Rational 		-- stop
	         (Rational -> a)	-- what to do, when, starting at start, stopping at stop.
					-- This allows <*> to be optimized, which is important.
	| Pure a


-- alts: duration, event, lifetime, time, clock
balloon :: Active UI
balloon = Active 0 1 f		-- TODO: check that this nevee
  where f n | n < 0     = error $ "ballon value negative" ++ show n
 	    | n > 1     = error $ "ballon value above unit (1)" ++ show n
	    | otherwise = fromRational n

actLerp :: (Lerp a) => Active UI -> a -> a -> Active a
actLerp (Active start stop f) a0 a1 = Active start stop (\ i -> lerp (f i) a0 a1)
actLerp (Pure i) a0 a1              = Pure $ lerp i a0 a1

-- Todo: fix this so as it is exact, and does not use toRational
-- Takes number of 'frames' per second.
simulate :: Float -> Active a -> [a]
simulate rate (Active start stop f) =  [ f step'
				       | step <- outerSteps (ceiling (toRational rate * (stop - start)))
				       , let step' = lerp step start stop
				       ] 
simulate rate (Pure a) 	        = [a]	-- need inf concept

instance Scale (Active a) where
  scale u (Active start stop f) = Active (scale u start) (scale u stop) 
				$ \ tm -> f (tm / toRational u)
  scale u (Pure a) 		= Pure a


actMove :: Float -> Active a -> Active a
actMove d (Active start stop f) = Active (toRational d + start) (toRational d + stop) 
				$ \ tm -> f (tm - toRational d)
actMove d (Pure a) = Pure a

instance Functor Active where
  fmap f (Active start stop g) = Active start stop (f . g)
  fmap f (Pure a) = Pure (f a)


boundBy :: Active a -> Rational -> Rational
boundBy (Active start stop _) i 
	| i <= start = start
 	| i >= stop  = stop
	| otherwise  = i

-- to real time
toTime :: Active a -> Rational -> Rational
toTime (Active low high _) r = lerp (fromRational r) low high

-- to UI time
fromTime :: Active a -> Rational -> Rational
fromTime (Active low high _) val = (val - low) / (high - low)

instance Applicative Active where
	pure a = Pure a
	(Pure a) <*> b = fmap a b
	(Active start0 stop0 f0) <*> (Pure a) = Active start0 stop0 (\ i -> (f0 i) a)
	a0@(Active start0 stop0 f0) <*> a1@(Active start1 stop1 f1) = 
		Active start stop $ \ i -> f0 (boundBy a0 i) (f1 (boundBy a1 i))
	   where
		start = min start0 start1
		stop  = max stop0 stop1

example2 :: Active (Board RGB)
example2 = fmap (scale 0.2)
	 $ fmap (choose red white .$)
	 $ flicker [ e1 , e2 , e3 , e4 ]
 where
	e1 = example1 (-1,1) (1,1) 0.1
	e2 = example1 (1,1) (1,-1) 0.1
	e3 = example1 (1,-1) (-1,-1) 0.1
	e4 = example1 (-1,-1) (-1,1) 0.1
	
example3 :: Active (Board RGB)
example3 = rot <*> example2
  where
	rot = fmap (\ ui -> rotate (pi * ui)) balloon `streach` example2
	
example1 :: Point -> Point -> R -> Active (Board Bool)
example1 s e th = id
 	$ fmap (\ (a,b) -> straightLine (s,(a,b)) th) 
	$ scale 5
	$ actLerp balloon s e

main = startChalkBoard [] $ \ cb -> do
	let loop (brd:rest) = do
		drawChalkBoard cb brd
		loop rest
	    loop [] = return ()

	startMyWriteStream cb "ffmpeg -f image2pipe -vcodec ppm -i - -vcodec libx264 -b 500k -vpre hq -vpre main square.mp4" 
	loop (simulate 30 example3)
	endWriteStream cb
	exitChalkBoard cb

{-
after :: Active a -> Active b -> Active b
after (Pure a) 
after (Active start0 stop0 _) (Active start1 stop1 b) =
	Active (
-}

data Era = Before | During | After
	deriving Show

sleep :: Float -> Active Era
sleep t = fmap (\ i -> if i <= 0 then Before
	 	  else if i >= 1 then After
		  else During) (scale t balloon)
	
zip :: Active a -> Active b -> Active (a,b)
zip a b = pure (,) <*> a <*> b

gzip = Graphics.ChalkBoard.Active.zip

instance Over a => Over (Active a) where
  over a1 a2 = fmap (\ (a,b) -> a `over` b) (zip a1 a2)

-- Note the different order; the second thing goes on top
lay :: Over a => Active a -> Active a -> Active a
lay a b = (b `after` a) `over` a

flicker :: Over a => [Active a] -> Active a
flicker = foldr1 lay

-------------------------------------------------
-- Time changers --------

-- Do *right* after second argument.
after :: Active a -> Active b -> Active a
after act@(Active low' _ _) (Active _ high _)  = actMove (fromRational (high - low')) act

-- Do *right* before the second argument
before :: Active a -> Active b -> Active a
before act@(Active _ high' _) (Active low _ _)  = actMove (fromRational (low - high')) act

-- Make the duration of the first argument
-- the same as the duration of the second argument.
streach :: Active a -> Active b -> Active a
streach act@(Active low' high' f) aux@(Active low high _) =
	Active low high $ \ tm -> f (toTime act (fromTime aux tm))

rev :: Active a -> Active a
rev (Active low high f) = Active low high $ \ tm -> f $ (high - tm) + low

--------------------------------------------------

fadein :: UI -> Active UI
fadein k = fmap (\ ui -> if ui < k then lerp (ui / k) 0 1 else 1) balloon


