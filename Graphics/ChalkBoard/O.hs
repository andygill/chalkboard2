{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances #-}
module Graphics.ChalkBoard.O ( -- * The Observable datatype
	  O	-- abstract
	, Obs(..)
	, OFunctor(..)
        , unO
	  -- * The Observable language
	, true, false
	, choose
	, alpha
	, withAlpha
	, unAlpha
	, transparent
	, red
	, green
	, blue
	, white
	, black
	, cyan
	, purple
	, yellow
	) where
	
import Graphics.ChalkBoard.Types as Ty
import Graphics.ChalkBoard.O.Internals as I
import qualified Graphics.ChalkBoard.Core as C
import Graphics.ChalkBoard.Expr as Expr


------------------------------------------------------------------------------------------------
-- Obs Class
------------------------------------------------------------------------------------------------

-- Applicative Functor like thing.
------------------------------------------------------------------------------------------------

class Obs a where
	-- construct an Observable
  	o :: a -> O a

infixl 4 <$>

class OFunctor f where
	(<$>) :: (O a -> O b) -> f a -> f b

------------------------------------------------------------------------------------------------
-- Projection
------------------------------------------------------------------------------------------------

-- | project into an unobservable version of O.
unO :: O o -> o
unO (O o _) = o

------------------------------------------------------------------------------------------------
-- Instances of Pure
------------------------------------------------------------------------------------------------

-- Are you allowed to say "Pure Bool"?
instance Obs Bool where
	o a = primO (O_Bool a) a

instance Obs RGB where
	o c = primO (O_RGB c) c

instance Obs RGBA where
	o c = primO (O_RGBA c) c


-- GADT attack
--lamO :: (O a -> O b) -> O (a -> b)
--lamO = Lam


-- | choose between two Observable alternatives, based on a Observable 'Bool'
choose :: O o -> O o -> O Bool -> O o
choose (O a ea) (O b eb) (O c ec)  = O (if c then a else b) (E $ Choose ea eb ec)

-- square :: Board (O Bool)

-- | Observable 'True'.
true :: O Bool
true  = primO (O_Bool True) True

-- | Observable 'False'.
false :: O Bool
false = primO (O_Bool False) False

------------------------------------------------------------------------------------------------
-- Functions from Core, lifted into the O type.
------------------------------------------------------------------------------------------------

-- | Observable function to add an alpha channel.
alpha :: O RGB -> O RGBA
alpha (O a e) = O (C.alpha a) (E $ Expr.Alpha 1 e)

-- | Observable function to add a preset alpha channel.
withAlpha :: UI -> O RGB -> O RGBA
withAlpha n (O a e) = O (C.alpha a) (E $ Expr.Alpha n e)

-- | Observable function to remove the alpha channel.
unAlpha :: O (RGBA) -> O RGB
unAlpha (O a e) = O (C.unAlpha a) (E $ Expr.UnAlpha e)

-- | Observable function to add a transparent alpha channel.
transparent :: O RGB -> O RGBA
transparent (O a e) = O (C.alpha a) (E $ Expr.Alpha 0 e)



red    :: O RGB
red    = o $ RGB 1.0 0.0 0.0
green  :: O RGB
green  = o $ RGB 0.0 1.0 0.0
blue   :: O RGB
blue   = o $ RGB 0.0 0.0 1.0
white  :: O RGB
white  = o $ RGB 1.0 1.0 1.0
black  :: O RGB
black  = o $ RGB 0.0 0.0 0.0
cyan   :: O RGB
cyan   = o $ RGB 0.0 1.0 1.0
purple :: O RGB
purple = o $ RGB 1.0 0.0 1.0
yellow :: O RGB
yellow = o $ RGB 1.0 1.0 0.0

