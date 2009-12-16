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

-- All the functions in our first order language.
data Expr s 
	= Choose s s s
	| O_Bool Bool
	| O_RGB RGB
	| O_RGBA RGBA -- (Ty.Alpha RGB)
	| O_Pair s s 			-- (a,b)
	| O_Fst s
	| O_Snd s
	| Lit R
	| Var Int
	| OrBool		-- the || function
	| Alpha UI s		-- O_Alpha?
	| ScaleAlpha UI s			-- RGBA -> RGBA
	| UnAlpha s
	| Hook String s			-- magic hook, to allow tunneling
	deriving Show

newtype E = E (Expr E)
	deriving Show

data ExprType = BOOL_Ty | RGB_Ty | RGBA_Ty	-- we seems to have this all over
	deriving (Show, Eq)

exprTypeE :: E -> Maybe ExprType
exprTypeE (E e) = exprType e

exprType :: Expr E -> Maybe ExprType
exprType (Choose _ a b)     = getFirst (First (exprTypeE a) `mappend` (First (exprTypeE b)))
exprType (O_Bool {})        = return BOOL_Ty
exprType (O_RGB {})         = return RGB_Ty
exprType (O_RGBA {}) 	    = return RGBA_Ty
exprType (Alpha {})	    = return RGBA_Ty
exprType (UnAlpha {})	    = return RGB_Ty
exprType (ScaleAlpha {})    = return RGBA_Ty
exprType (Hook {})    	    = return RGB_Ty
exprType _                  = Nothing

exprUnifyE :: E -> ExprType -> [(Int,ExprType)]
exprUnifyE (E e) = exprUnify e

-- exprUnify :: what the expected result type is, and does it unify
exprUnify :: Expr E -> ExprType -> [(Int,ExprType)]
exprUnify (Choose a b c) ty = L.nub (exprUnifyE a ty ++ exprUnifyE b ty ++ exprUnifyE c BOOL_Ty)
exprUnify (O_Bool {}) BOOL_Ty = []
exprUnify (O_RGB {}) RGB_Ty = []
exprUnify (O_RGBA {}) RGBA_Ty = []
exprUnify (Alpha _ e) RGBA_Ty = exprUnifyE e RGB_Ty
exprUnify (UnAlpha e) RGB_Ty = exprUnifyE e RGBA_Ty
exprUnify (ScaleAlpha _ e) RGBA_Ty = exprUnifyE e RGBA_Ty
exprUnify (Hook _ e)  RGB_Ty = exprUnifyE e RGB_Ty
exprUnify (Var i) ty = [(i,ty)]
exprUnify other ty = error $ "exprUnify" ++ show (other,ty)


-- evaluate to a normal form (constant folding, really)
evalExprE :: Expr E -> Maybe (Expr E)
-- already values
evalExprE e@(Var {}) 		= return e
evalExprE e@(O_Bool {}) 	= return e
evalExprE e@(O_RGB {}) 	= return e
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

unE :: E -> Expr E
unE (E e) = e

evalE :: E -> Maybe E
evalE (E e) = liftM E (evalExprE e)

-- The generic plubing for our Expr datatype.				-- 

instance MuRef E where
  type DeRef E = Expr
  mapDeRef f (E e) = T.traverse f e


instance T.Traversable Expr where
	traverse f (Choose a b c) 	= Choose <$> f a <*> f b <*> f c
	traverse f (Alpha c e) 		= Alpha c <$> f e
	traverse f (UnAlpha e) 		= UnAlpha <$> f e
	traverse f (ScaleAlpha c e) 	= ScaleAlpha c <$> f e
	traverse f (O_Bool v)		= pure $ O_Bool v
	traverse f (O_RGB v)		= pure $ O_RGB v
	traverse f (Lit r)		= pure $ Lit r
	traverse f (Var i)		= pure $ Var i
	traverse f (O_RGBA v)		= pure $ O_RGBA v
	traverse f (Hook s v)           = pure (Hook s) <*> f v
	-- TODO
	
instance F.Foldable Expr where
	foldMap f (Choose a b c) = mconcat [f a, f b, f c]
	--- TODO
	
instance Functor Expr where
	fmap f (Choose a b c) = Choose (f a) (f b) (f c)
	--- TODO
