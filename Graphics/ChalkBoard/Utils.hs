-- |
-- Module: Graphics.Chalkboard.Utils
-- Copyright: (c) 2009 The University of Kansas
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This module has some basic, externally visable, definitions.

module Graphics.ChalkBoard.Utils 
	( -- * Point Utilties.
	  insideRegion
	, insideCircle
	, distance
	, intervalOnLine
	, circleOfDots
	, insidePoly
	  -- * Utilties for @R@.
	, innerSteps, outerSteps, fracPart
	, fromPolar
	, toPolar
	, angleOfLine
	, 
	) where

--	, red, green, blue, white, black, cyan, purple, yellow
import Graphics.ChalkBoard.Types

-- | innerSteps takes n even steps from 0 .. 1, by not actually touching 0 or 1.
-- The first and last step are 1/2 the size of the others, so that repeated innerSteps
-- can be tiled neatly.
innerSteps :: Int -> [R]
innerSteps n = map (/ fromIntegral (n * 2)) (map fromIntegral (take n [1::Int,3..]))

-- | outerSteps takes n even steps from 0 .. 1, starting with 0, and ending with 1,
--  returning n+1 elements.

outerSteps :: Int -> [R]
outerSteps n = map (/ fromIntegral n) (map fromIntegral (take (n + 1) [(0::Int)..]))

-- | Extract the fractional part of an @R@.
fracPart :: R -> R
fracPart x = x - fromIntegral ((floor x) :: Integer)

-- Point operations

-- | is a @Point@ inside a region?

insideRegion :: (Point,Point) -> Point -> Bool
insideRegion ((x1,y1),(x2,y2)) (x,y) = x1 <= x && x <= x2
	  		           && y1 <= y && y <= y2 

-- | is a @Point@ inside a circle, where the first two arguments are the center of the circle,
-- and the radius.


insideCircle :: Point -> R -> Point -> Bool
insideCircle (x1,y1) r (x,y) = distance (x1,y1) (x,y) <= r


-- | What is the 'distance' between two points in R2?
-- This is optimised for the normal form @distance p1 p2 <= v@, which avoids using @sqrt@.

distance :: Point -> Point -> R
distance (x,y) (x',y') = sqrt (xd * xd + yd * yd)
  where
	xd = x - x'
	yd = y - y'

{-# INLINE distance #-}
-- The obvious sqrt (x * x) ==> x does not fire.	
{-# RULES "distance <= w" forall t u w . distance t u <= w = distanceLe t u w #-}
{-# INLINE distanceLe #-}
distanceLe :: Point -> Point -> R -> Bool
distanceLe (x,y) (x',y') w = (xd * xd + yd * yd) <= w * w
  where
	xd = x - x'
	yd = y - y'


-- | 'intervalOnLine' find the place on a line (between 0 and 1) that is closest to the given point.
intervalOnLine :: (Point,Point) -> Point -> R 
intervalOnLine  ((x1,y1),(x2,y2)) (x0,y0) =
	((x0-x1)*(x2-x1)+(y0-y1)*(y2-y1))/(xd * xd + yd * yd)		
  where
		xd = x2-x1
		yd = y2-y1

fromPolar :: (R,Radian) -> Point 
fromPolar (p, phi) = (p * cos phi, p * sin phi) 

toPolar :: Point -> (R,Radian)
toPolar (x, y) = (sqrt (x * x + y * y), atan2 y x)

angleOfLine :: (Point,Point) -> Radian
angleOfLine ((x1,y1),(x2,y2)) = atan2 (x2 - x1) (y2 - y1)


-- | circleOfDots generates a set of points between (-1..1,-1..1), inside a circle.
circleOfDots :: Int -> [Point]
circleOfDots 0 = error "circleOfDots 0"
circleOfDots n = [ (x,y)
		 | x <- map (\ t -> t * 2 - 1) $ outerSteps n
		 , y <- map (\ t -> t * 2 - 1) $ outerSteps n
		 , (x * x + y * y) <= 1.0 
		 ]

insidePoly :: [Point] -> Point -> Bool
insidePoly nodes (x,y) 
	  -- no numbers above 0, or no numbers below zero
	  -- means that the numbers we the *same* sign (or zero)>
	| null (filter (> 0) vals) ||  null (filter (< 0) vals) = True
	| otherwise		   				= False
  where
	vals = [ (y - y0) * (x1 - x0) - (x - x0) * (y1 - y0)
	       | ((x0,y0),(x1,y1)) <- zip nodes (tail nodes ++ [head nodes]) 
 	       ]

