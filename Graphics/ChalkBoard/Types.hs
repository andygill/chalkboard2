{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module: Graphics.ChalkBoard.Types
-- Copyright: (c) 2009 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This module contains the types used by chalkboard, except Board itself.
--
module Graphics.ChalkBoard.Types 
	( -- * Basic types
	  UI, R, Point, Radian,
	  -- * Overlaying
	  Over(..),
	  stack,
	  -- * Scaling
	  Scale(..),
	  -- * Linear Interpolation
	  Lerp(..),
	  -- * Averaging
	  Average(..),
{-
	  -- * Alpha Channel support
	  Alpha(..),
	 alpha, transparent, withAlpha, unAlpha,
	  -- * Z buffer support
	  Z(..),
-}
	  -- * Constants
	  nearZero
	  -- * Colors
	, Gray
	, RGB(..)
	, RGBA(..)
	) where

import Data.Binary
import Control.Monad

-- | A real number.
type R = Float

-- | Unit Interval: value between 0 and 1, inclusive.
type UI = R

-- | A point in R2.
type Point = (R,R)

-- | Angle units
type Radian = Float	

-- | Close to zero; needed for @Over (Alpha c)@ instance.
nearZero :: R
nearZero = 0.0000001 


------------------------------------------------------------------------------

infixr 5 `over`

-- | For placing a value literally /over/ another value. The 2nd value /might/ shine through.
-- The operation /must/ be associative.
class Over c where
  over :: c -> c -> c

instance Over Bool where
  over = (||)

instance Over (Maybe a) where
  (Just a) `over` _    = Just a
  Nothing `over` other = other

-- | 'stack' stacks a list of things over each other, 
-- where earlier elements are 'over' later elements.
-- Requires non empty lists, which can be satisfied by using an explicitly
-- transparent @Board@ as one of the elements.

stack :: (Over c) => [c] -> c
stack = foldr1 over

------------------------------------------------------------------------------

-- | 'Scale' something by a value. scaling value can be bigger than 1.

class Scale c where
  scale :: R -> c -> c

instance Scale R where
  scale u v = u * v

instance (Scale a,Scale b) => Scale (a,b) where
  scale u (x,y) = (scale u x,scale u y)


------------------------------------------------------------------------------

-- | Linear interpolation between two values.
class Lerp a where
  lerp :: UI -> a -> a -> a

instance Lerp R where
  lerp s v v' = v + (s * (v' - v))

-- | 'Lerp' over pairs

instance (Lerp a,Lerp b) => Lerp (a,b) where
  lerp s (a,b) (a',b') = (lerp s a a',lerp s b b')

instance (Lerp a) => Lerp (Maybe a) where
  lerp _ Nothing  Nothing  = Nothing
  lerp _ (Just a) Nothing  = Just a 
  lerp _ Nothing  (Just b) = Just b
  lerp s (Just a) (Just b) = Just (lerp s a b)

------------------------------------------------------------------------------

-- | 'Average' a set of values. weighting can be achived using multiple entries.

class Average a where
  -- | average is not defined for empty list
  average :: [a] -> a 

instance Average R where
  average xs = sum xs / fromIntegral (length xs)
  
instance (Average a,Average b) => Average (a,b) where
  average xs = (average $ map fst xs,average $ map snd xs)

------------------------------------------------------------------------------

-- | 'Gray' is just a value between 0 and 1, inclusive.
-- Be careful to consider if this is pre or post gamma.
type Gray = UI

instance Over Gray where
  over r _ = r

------------------------------------------------------------------------------
-- Simple colors

-- | 'RGB' is our color, with values between 0 and 1, inclusive.
data RGB  = RGB !UI !UI !UI deriving Show

instance Over RGB where
  -- simple overwriting
  over x _y = x

instance Lerp RGB where
  lerp s (RGB r g b) (RGB r' g' b')
	 = RGB (lerp s r r') 
	       (lerp s g g')
	       (lerp s b b')

instance Scale RGB where
  scale s (RGB r g b)
	 = RGB (scale s r) 
	       (scale s g)
	       (scale s b) 

instance Average RGB where
  average cs = RGB (average reds) (average greens) (average blues)
     where
	reds   = [ r | RGB r _ _ <- cs ]
	greens = [ g | RGB _ g _ <- cs ]
	blues  = [ b | RGB _ _ b <- cs ]


-- Consider using 4 bytes for color, rather than 32 bytes for the double (or are they floats?)
instance Binary RGB where
  put (RGB r g b) = put r >> put g >> put b
  get = liftM3 RGB get get get


------------------------------------------------------------------------------------------
-- Colors with alpha

-- | 'RGBA' is our color, with values between 0 and 1, inclusive.
-- These values are *not* prenormalized
data RGBA = RGBA !UI !UI !UI !UI deriving Show


 -- Todo: rethink what this means

instance Over RGBA where
  over (RGBA r g b a) (RGBA r' g' b' a') =
	RGBA (f r r')
	     (f g g')
	     (f b b')
	     (a * a')
    where f x y = a * y + (1 - a) * x

{-
   -- An associative algorithm for handling the alpha channel
	-- Associative; please reinstate later

	| a <= nearZero = RGBA r' g' b' a_new
	| otherwise     = RGBA 
			(lerp r' (scale (1/a) r) a) 
			(lerp g' (scale (1/a) g) a) 
			(lerp b' (scale (1/a) b) a) 
				 a_new
     where
	-- can a_new be 0? only if a == 0 and a' == 0
	a_new     = a + a' * (1 - a)
-}

instance Binary RGBA where
  put (RGBA r g b a) = put r >> put g >> put b >> put a
  get = liftM4 RGBA get get get get
