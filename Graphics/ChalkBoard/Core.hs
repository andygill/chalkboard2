{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.ChalkBoard.Core 
	( -- * Pointwise operators.
	  interpBool 
	, sampleUI
	, withMask
	, withDefault
--	, choose
	  -- * Colors
	, alpha, withAlpha, unAlpha, transparent
	) where

-- This provided the internal functions, many of which are reflected into the "Observable", O.

import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.Utils

------------------------------------------------------------------------------

-- Pointwise operators

-- | Covert a Bool (mask point) into a unit interval.
interpBool :: Bool -> UI
interpBool True  = 1.0
interpBool False = 0.0

-- | sample a UI, giving a point of a mask.
sampleUI :: UI -> Bool
sampleUI n = n >= 0.5

-- | Use a mask to create a Just value, or Nothing.
withMask :: a -> (Bool -> Maybe a)
withMask a True  = Just a
withMask _ False = Nothing

-- | With a default if you do not have a Just.
withDefault :: a -> (Maybe a -> a)
withDefault a Nothing  = a
withDefault _ (Just b) = b

-- | 'choose' between two values.
choose :: a -> a -> (Bool -> a)
choose t  _f True  = t
choose _t f  False = f

------------------------------------------------------------------------------------------
-- Colors with alpha

alpha :: RGB -> (RGBA -> RGBA)
alpha (RGB r g b) _ = RGBA r g b 1

withAlpha :: RGB -> UI -> (RGBA -> RGBA)
withAlpha (RGB r g b) a rgba = (RGBA r g b a) `over` rgba

unAlpha :: RGB -> (RGBA -> RGBA) -> RGB
unAlpha (RGB r g b) f = RGB (r' * a') (g' * a') (b' * a')
  where
	(RGBA r' g' b' a') = f (RGBA r g b 1)

transparent :: RGBA -> RGBA
transparent = id
