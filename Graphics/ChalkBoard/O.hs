{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses  #-}
module Graphics.ChalkBoard.O ( -- * The Observable datatype
	  O	-- abstract
	, Obs(..)
	, OFunctor(..)
        , unO
	  -- * The Observable language
	, true, false
	, choose
	, mix
	, alpha
	, withAlpha
--	, unAlpha	 -- for now, until we figure out how to compile it
	, transparent
	, red
	, green
	, blue
	, white
	, black
	, cyan
	, purple
	, yellow
	, withMask
	, Graphics.ChalkBoard.O.fstO
	, Graphics.ChalkBoard.O.sndO
	, withDefault
	, Boolean(..)
	, Maskable
	) where
	
import Graphics.ChalkBoard.Types as Ty
import Graphics.ChalkBoard.O.Internals as I
import qualified Graphics.ChalkBoard.Core as C
import Graphics.ChalkBoard.Expr as Expr
import Data.Boolean

------------------------------------------------------------------------------------------------
-- Obs Class
------------------------------------------------------------------------------------------------

-- Applicative Functor like thing.
------------------------------------------------------------------------------------------------

class Obs a where
	-- construct an Observable
  	o :: a -> O a

infixr 0 <$>

class OFunctor f where
	(<$>) :: (O a -> O b) -> f a -> f b
--	ozip  :: f a -> f b -> f (a,b)

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
	o a = primO (E BOOL_Ty $ O_Bool a) a

instance Obs RGB where
	o c = primO (E RGB_Ty $ O_RGB c) c

--instance Obs RGBA where
--	o c = primO (O_RGBA c) c

instance Obs UI where
	o c = primO (E UI_Ty $ Lit c) c

-- GADT attack
--lamO :: (O a -> O b) -> O (a -> b)
--lamO = Lam


-- | choose between two Observable alternatives, based on a Observable 'Bool'
choose :: O o -> O o -> O Bool -> O o
choose (O a ea) (O b eb) (O c ec)  = O (if c then a else b) (E (unifyTy (typeE ea) (typeE eb)) $ Choose ea eb ec)

mix :: (Lerp o) => O o -> O o -> O UI -> O o
mix (O a ea) (O b eb) (O c ec)  = O (lerp c a b) (E (unifyTy (typeE ea) (typeE eb)) $ Mix ea eb ec)

------------------------------------------------------------------------------------------------
-- Functions from Core, lifted into the O type.
------------------------------------------------------------------------------------------------

-- | Observable function to add an alpha channel.
alpha :: O RGB -> O (RGBA -> RGBA)
alpha (O a e) = O (C.alpha a) (E RGBA_Ty $ Expr.Alpha 1 e)

-- | Observable function to add a preset alpha channel.
withAlpha :: UI -> O RGB -> O (RGBA -> RGBA)
withAlpha n (O a e) = O (C.alpha a) (E RGBA_Ty $ Expr.Alpha n e)

-- | Observable function to remove the alpha channel.
--unAlpha :: O RGB -> O (RGBA -> RGBA) -> O RGB
--unAlpha (O a1 e1) (O a2 e2) = O (C.unAlpha a1 a2) (E $ Expr.UnAlpha e1 e2)

-- | Observable function to add a transparent alpha channel.
transparent :: O (RGBA -> RGBA)
transparent = O id (E RGBA_Ty $ Expr.Alpha 0 (E RGBA_Ty $ O_RGB (RGB 0 0 0)))

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


-- TODO: generalize to UI as well as RGB

class Maskable a 

--     maskable :: a -> ExprType

instance Maskable RGB

instance Maskable UI

withMask :: Maskable a => O a -> O Bool -> O (Maybe a)
withMask (O a ea) (O b eb) = O (C.withMask a b) (E (Maybe_Ty (typeE ea)) $ WithMask ea eb)

withDefault :: O a -> O (Maybe a) -> O a
withDefault (O a ea) (O b eb) = O (C.withDefault a b) (E (unifyTy (typeE ea) (unMaybe (typeE eb))) $ WithDefault ea eb)
  where unMaybe (Maybe_Ty ty) = ty
	unMaybe _ = error "failed type extraction, expecting Maybe"

fstO :: O (a,b) -> O a
fstO (O ~(a,_) e) = O a (oFst e)

sndO :: O (a,b) -> O b
sndO (O ~(_,b) e) = O b (oSnd e)

instance Boolean (O Bool) where
	true = o True
	false = o False
	notB (O a ea) = O (not a) $ E BOOL_Ty $ NOT ea
	-- (&&*) :: b -> b -> b
	-- (||*) :: b -> b -> b
	

instance IfB (O Bool) (O a) where
	ifB c t e = choose t e c

instance Eq a => EqB (O Bool) (O a) where
	(==*) (O a ea) (O b eb) = O (a == b) $ E BOOL_Ty $ EQUAL ea eb

		
instance OrdB (O Bool) (O a) where
	(<*) = error "" -- :: a -> a -> bool
	(>=*) = error "" --(>=*) :: a -> a -> bool
	(>*) = error "" --(>*) :: a -> a -> bool
	(<=*) = error "" --(<=*) :: a -> a -> bool	

--nothing :: O (Maybe a)
--nothing = O Nothing (O $ E