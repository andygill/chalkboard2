{-# LANGUAGE TypeFamilies, GADTs #-}
module Graphics.ChalkBoard.Expr where
	
--import Control.Applicative
import Graphics.ChalkBoard.Types -- hiding (Alpha)
import qualified Graphics.ChalkBoard.Types as Ty
import Graphics.ChalkBoard.Core as C
import Data.Reify.Graph
import Data.Reify
import Control.Applicative as AF
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid
import Data.Maybe
import qualified Data.List as L
import Control.Monad

------------------------------------------------------------------------------
-- Data Type
------------------------------------------------------------------------------

-- All the functions in our first order language.
data Expr s 
	-- The var
	= Var Path	
	-- constants
	| O_Bool Bool
	| O_RGB RGB
	| O_RGBA RGBA -- (Ty.Alpha RGB)
	| Lit R
	-- deconstructors
	| O_Fst s
	| O_Snd s
	-- constructors
	| O_Pair s s 			-- (a,b)
	-- Functions
	| OrBool			-- the || function
	| Choose s s s			-- O a -> O a -> O Bool -> O a
	| Alpha UI s			-- O_Alpha?
	| ScaleAlpha UI s		-- RGBA -> RGBA
	| UnAlpha s
	| Hook String s			-- magic hook, to allow tunneling
	| Hook2 String s s
        | WithMask s s			-- O a -> O Bool 	-> O (Maybe a)
	| WithDefault s s		-- O a -> O (Maybe a) 	-> O a
	deriving Show

data Path = Here | Left Path | Right Path
	deriving (Show,Eq,Ord)

newtype E = E (Expr E)
	deriving Show

unE :: E -> Expr E
unE (E e) = e

------------------------------------------------------------------------------
-- Unification and Type Checking
------------------------------------------------------------------------------

data ExprType 
	= BOOL_Ty 
	| RGB_Ty 
	| RGBA_Ty 
	| Pair_Ty ExprType ExprType
	| Maybe_Ty ExprType
    deriving (Show, Eq)

exprUnifyE :: E -> ExprType -> [(Path,ExprType)]
exprUnifyE (E e) = exprUnify e

-- exprUnify :: what the expected result type is, and does it unify
exprUnify :: Expr E -> ExprType -> [(Path,ExprType)]
exprUnify (Choose a b c) 	ty 		= L.nub (exprUnifyE a ty ++ exprUnifyE b ty ++ exprUnifyE c BOOL_Ty)
exprUnify (O_Bool {}) 		BOOL_Ty 	= []
exprUnify (O_RGB {}) 		RGB_Ty 		= []
exprUnify (O_RGBA {}) 		RGBA_Ty 	= []
exprUnify (Alpha _ e) 		RGBA_Ty 	= exprUnifyE e RGB_Ty
exprUnify (UnAlpha e) 		RGB_Ty 		= exprUnifyE e RGBA_Ty
exprUnify (ScaleAlpha _ e) 	RGBA_Ty 	= exprUnifyE e RGBA_Ty
exprUnify (Hook _ e) 		RGB_Ty 		= exprUnifyE e (Pair_Ty RGB_Ty RGB_Ty)
exprUnify (WithMask e1 e2) 	(Maybe_Ty ty) 	= L.nub (exprUnifyE e1 ty ++ exprUnifyE e2 BOOL_Ty)
exprUnify (WithDefault e1 e2) 	ty 		= L.nub (exprUnifyE e1 ty ++ exprUnifyE e2 (Maybe_Ty ty))
exprUnify (Var i) 		ty 		= [(i,ty)]
exprUnify other ty = error $ "exprUnify failure (internal errror) " ++ show (other,ty)

------------------------------------------------------------------------------
-- Evaluation
------------------------------------------------------------------------------

-- TODO: consider using the shallow embedding for this.
-- evaluate to a normal form (constant folding, really)
evalExprE :: Expr E -> Maybe (Expr E)
-- already values
evalExprE e@(Var {}) 		= return e
evalExprE e@(O_Bool {}) 	= return e
evalExprE e@(O_RGB {}) 		= return e
evalExprE e@(O_RGBA {}) 	= return e
-- try some evaluation, please.
evalExprE (Choose a b c) = 
	case liftM unE $ evalE c of
	  Just (O_Bool True)  -> liftM unE $ evalE a
	  Just (O_Bool False) -> liftM unE $ evalE b
	  other -> Nothing
evalExprE (Alpha a e) = 
	case liftM unE $ evalE e of
	    Just (O_RGB c) -> return $ O_RGBA (C.withAlpha c a)
	    other -> Nothing
evalExprE other = Nothing

evalE :: E -> Maybe E
evalE (E e) = liftM E (evalExprE e)

------------------------------------------------------------------------------
-- Reification Support
------------------------------------------------------------------------------

-- The generic plubing for our Expr datatype.				-- 

instance MuRef E where
  type DeRef E = Expr
  mapDeRef f (E e) = T.traverse f e



instance T.Traversable Expr where
	traverse f (Var i)		= pure $ Var i

	traverse f (O_Bool v)		= pure $ O_Bool v
	traverse f (O_RGB v)		= pure $ O_RGB v
	traverse f (O_RGBA v)		= pure $ O_RGBA v
	traverse f (Lit r)		= pure $ Lit r

	traverse f (O_Fst a) 		= O_Fst <$> f a
	traverse f (O_Snd a) 		= O_Snd <$> f a

	traverse f (Choose a b c) 	= Choose <$> f a <*> f b <*> f c
	traverse f (Alpha c e) 		= Alpha c <$> f e
	traverse f (ScaleAlpha c e) 	= ScaleAlpha c <$> f e
	traverse f (UnAlpha e) 		= UnAlpha <$> f e

	traverse f (Hook s v)           = pure (Hook s) <*> f v
	traverse f (Hook2 s v1 v2)      = pure (Hook2 s) <*> f v1 <*> f v2

        traverse f (WithMask v1 v2)	= pure WithMask <*> f v1 <*> f v2
        traverse f (WithDefault v1 v2)	= pure WithDefault <*> f v1 <*> f v2
	-- TODO
	
instance F.Foldable Expr where
	foldMap f (Choose a b c) = mconcat [f a, f b, f c]
	--- TODO
	
instance Functor Expr where
	fmap f (Choose a b c) = Choose (f a) (f b) (f c)
	--- TODO
