module Graphics.ChalkBoard.Active where
	
import Control.Applicative
import Graphics.ChalkBoard.Types 
import Graphics.ChalkBoard.Utils
import Data.Ratio
import Data.Time.Clock
import Control.Concurrent.MVar

data Active a 
	= Active Rational		-- start
	         Rational 		-- stop
	         (Rational -> a)	-- what to do, when, starting at start, stopping at stop.
					-- This allows <*> to be optimized, which is important.
	| Pure a


-- alts: duration, event, lifetime, time, clock
age :: Active UI
age = Active 0 1 f		-- TODO: check that this nevee
  where f n | n < 0     = error $ "ballon value negative" ++ show n
 	    | n > 1     = error $ "ballon value above unit (1)" ++ show n
	    | otherwise = fromRational n

actLerp :: (Lerp a) => Active UI -> a -> a -> Active a
actLerp (Active start stop f) a0 a1 = Active start stop (\ i -> lerp (f i) a0 a1)
actLerp (Pure i) a0 a1              = Pure $ lerp i a0 a1

travel :: (Lerp a) => a -> a -> Active a
travel = actLerp age

-- Todo: fix this so as it is exact, and does not use toRational
-- Takes number of 'frames' per second.
simulate :: Float -> Active a -> [a]
simulate rate (Active start stop f) =  [ f step'
				       | step <- outerSteps (ceiling (toRational rate * (stop - start)))
				       , let step' = lerp step start stop
				       ] 
simulate _ (Pure a) 	        = [a]	-- need inf concept

instance Scale (Active a) where
  scale u (Active start stop f) = Active (scale u start) (scale u stop) 
				$ \ tm -> f (tm / toRational u)
  scale _ (Pure a) 		= Pure a


actMove :: Float -> Active a -> Active a
actMove d (Active start stop f) = Active (toRational d + start) (toRational d + stop) 
				$ \ tm -> f (tm - toRational d)
actMove _ (Pure a) = Pure a

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

data Era = Before | During | After
	deriving Show

sleep :: Float -> Active Era
sleep t = fmap (\ i -> if i <= 0 then Before
	 	  else if i >= 1 then After
		  else During) (scale t age)
	
both :: Active a -> Active b -> Active (a,b)
both a b = pure (,) <*> a <*> b

instance Over a => Over (Active a) where
  over a1 a2 = fmap (\ (a,b) -> a `over` b) (both a1 a2)

infixr 5 `lay`
-- Note the different order; the second thing goes on top
lay :: Over a => Active a -> Active a -> Active a
lay a b = (b `after` a) `over` a

flicker :: Over a => [Active a] -> Active a
flicker = foldr1 lay

-------------------------------------------------

--page :: Active a -> Active a -> Active a



-------------------------------------------------
-- Time changers --------

-- Do *right* after second argument.
after :: Active a -> Active b -> Active a
after act@(Active low' _ _) (Active _ high _)  = actMove (fromRational (high - low')) act

-- Do *right* before the second argument
before :: Active a -> Active b -> Active a
before act@(Active _ high' _) (Active low _ _)  = actMove (fromRational (low - high')) act

-- add duration.
duration :: Active a -> Active b -> Active ()
duration a b = (const ()) `fmap` (a `both` b)

-- Make the duration of the first argument
-- the same as the duration of the second argument.
streach :: Active a -> Active b -> Active a
streach act@(Active _ _ f) aux@(Active low high _) =
	Active low high $ \ tm -> f (toTime act (fromTime aux tm))
streach (Pure a) (Active low high _) =
	Active low high $ \ _ -> a

reverse :: Active a -> Active a
reverse (Active low high f) = Active low high $ \ tm -> f $ (high - tm) + low

infixr 5 `page`
page :: Active a -> Active a -> Active a
page (Active low high f) (Active low' high' f') =
	Active low (high' + delta) $ \ tm ->
		if tm <= high
		then f tm
		else f' (tm - delta)
  where
     delta = high - low'
 
instance Show a => Show (Active a) where
	show (Pure a) = "pure (" ++ show a ++ ")"
	show (Active low high _) = "active (" ++ show low ++ ") (" ++ show high ++ ")"

turnPages :: Active (a -> a -> a) -> [Active a] -> Active a
turnPages turn (x:y:xs) = x `page` (turn <*> (snap 1 x) <*>  (snap 0 y)) `page` turnPages turn (y:xs)
turnPages _ [x] = x

-- Freeze an active value in time.
snap :: UI -> Active a -> Active a
snap _ (Pure a) 	      = Pure a
snap ui a@(Active _ _ f) = Pure $ f (toTime a (toRational ui))

transition :: Active (a -> a -> a) -> Active a -> Active a -> Active a
transition merge s1 s2 = merge <*> (snap 1 s1) <*> (snap 0 s2)

--------------------------------------------------

lerpAct :: (Lerp a) => Active (a -> a -> a)
lerpAct = Active 0 1 (\ tm -> lerp (fromRational tm))


-- 0..1 in the first k fraction, then 1 from then onwards.
fadein :: UI -> Active UI
fadein k = scale k age
--	fmap (\ ui -> if ui < k then lerp (ui / k) 0 1 else 1) age


-----------------------------------------------

-- Take a movie of what an Active object does between two type stamps.
movie :: R -> R -> Active a -> Active a
movie start stop act@(Active _ _ f) = Active (toRational start) (toRational stop) $ f . boundBy act
movie start stop (Pure a) 		     = Active (toRational start) (toRational stop) $ const a

-- | 'for' takes short movie of the Active argument, assumping starting at zero.
-- concept: 'for 44 (...)' runs '(...)' for 44 seconds.
for :: R -> Active a -> Active a
for n a = movie 0 n a
-- 
-- 
taking :: R -> Active a -> Active a
taking n a = streach a (sleep n)



--------

data Player a = RealTime UTCTime (Active a)
	      | Frame Rational (MVar Integer) (Active a)

realTime :: Active a -> IO (Player a)
realTime act = do
	tm <- getCurrentTime
	return $ RealTime tm act
	
-- 	
byFrame :: Rational -> Active a -> IO (Player a)
byFrame fps act = do
	v <- newMVar 0
	return $ Frame (1 / fps) v act
	
play :: Player a -> IO (Maybe a)
play (RealTime _ (Pure a))= return (Just a)
play (RealTime tm act) = do
	tm' <- getCurrentTime
	let r :: Rational
	    r = toRational (diffUTCTime tm' tm)
	return $ snapAt r act
play (Frame _ _ (Pure a)) = return (Just a)
play (Frame rate frameVar act) = do
	frame <- takeMVar frameVar 
	putMVar frameVar (frame + 1)
	let r :: Rational
	    r = rate * toRational frame
	return $ snapAt r act

-- Internal function, only works *inside* the motion-live of an object.
snapAt :: Rational -> Active a -> Maybe a
snapAt r act@(Active start end f) 
	| r >= start && r <= end = Just $ f (boundBy act r)
	| otherwise              = Nothing




