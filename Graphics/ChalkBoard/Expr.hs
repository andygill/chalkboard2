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
	= Var [Path]
	-- constants TODO: common up the constants
	| Lit R
	| O_Bool Bool
	| O_RGB RGB
	| O_RGBA RGBA -- (Ty.Alpha RGB)
	| O_Nothing
	-- deconstructors
--	| O_Fst s
--	| O_Snd s
	-- constructors
--	| O_Pair s s 			-- (a,b)
	-- Functions
	| O_Just s			-- 
--	| IsJust s			-- O (Just a) -> O Bool
	| OrBool			-- the || function
	| Choose s s s			-- O a -> O a -> O Bool -> O a
        | Mix s s s 			-- O a -> O a -> O UI -> O a
	| Alpha UI s			-- O_Alpha?
	| ScaleAlpha UI s		-- RGBA -> RGBA	-- TODO - is the dead code
	| UnAlpha s s			-- RGB -> (RGBA -> RGBA) -> RGB
        | WithMask s s			-- O a -> O Bool 	-> O (Maybe a)
	| WithDefault s s		-- O a -> O (Maybe a) 	-> O a

	-- boolean ops
	| EQUAL s s			-- O a -> O a -> O Bool
	| NOT s				-- O Bool -> O Bool
	deriving Show

data Path  = GoLeft | GoRight 
	deriving (Show,Eq,Ord)
	
data E = E ExprType (Expr E)		-- TODO: changing type of E
	deriving Show

unE :: E -> Expr E
unE (E _ e) = e

typeE :: E -> ExprType
typeE (E t _) = t

------------------------------------------------------------------------------
-- Unification and Type Checking
------------------------------------------------------------------------------

data ExprType 
	= BOOL_Ty 
	| RGB_Ty 
	| RGBA_Ty 	-- Change to RGBA_to_RGBA_Ty
	| UI_Ty
	| Pair_Ty ExprType ExprType
	| Left_Ty ExprType			-- ( ty, \alpha )
	| Right_Ty ExprType			-- ( \alpha, ty )
	| Maybe_Ty ExprType
	| Poly_Ty		-- because of fst, snd
    deriving (Show, Eq)

-- SHOULD BE DEAD CODE!
exprUnifyE :: E -> ExprType -> [([Path],ExprType)]
exprUnifyE (E _ e) = exprUnify e

-- exprUnify :: what the expected result type is, and does it unify
exprUnify :: Expr E -> ExprType -> [([Path],ExprType)]
exprUnify (Choose a b c) 	ty 		= L.nub (exprUnifyE a ty ++ exprUnifyE b ty ++ exprUnifyE c BOOL_Ty)
exprUnify (Mix a b c) 	ty 			= L.nub (exprUnifyE a ty ++ exprUnifyE b ty ++ exprUnifyE c UI_Ty)
exprUnify (O_Bool {}) 		BOOL_Ty 	= []
exprUnify (O_RGB {}) 		RGB_Ty 		= []
exprUnify (O_RGBA {}) 		RGBA_Ty 	= []
exprUnify (Lit {}) 		UI_Ty 		= []
exprUnify (Alpha _ e) 		RGBA_Ty 	= exprUnifyE e RGB_Ty
exprUnify (UnAlpha e1 e2) 	RGB_Ty 		= L.nub (exprUnifyE e1 RGB_Ty ++ exprUnifyE e2 RGBA_Ty)
exprUnify (ScaleAlpha _ e) 	RGBA_Ty 	= exprUnifyE e RGBA_Ty
exprUnify (WithMask e1 e2) 	(Maybe_Ty ty) 	= L.nub (exprUnifyE e1 ty ++ exprUnifyE e2 BOOL_Ty)
exprUnify (WithDefault e1 e2) 	ty 		= L.nub (exprUnifyE e1 ty ++ exprUnifyE e2 (Maybe_Ty ty))

exprUnify (Var i) 		ty 		= [(i,ty)]
--exprUnify (O_Fst e) 		ty 		= exprUnifyE e (Pair_Ty ty Poly_Ty)
--exprUnify (O_Snd e) 		ty 		= exprUnifyE e (Pair_Ty Poly_Ty ty)
exprUnify other ty = error $ "exprUnify failure (internal errror) " ++ show (other,ty)

unifyTy :: ExprType -> ExprType -> ExprType
unifyTy t1 t2 
  | t1 == t2 = t1
  | otherwise = error $ "unifyTy failed" ++ show (t1,t2)
	
------------------------------------------------------------------------------
-- constructors
------------------------------------------------------------------------------

oFst :: E -> E
oFst (E (Pair_Ty ty _) (Var i)) = E ty (Var (i ++ [GoLeft]))
oFst other   = error $ "oFst failed" ++ show other

oSnd :: E -> E
oSnd (E (Pair_Ty _ ty) (Var i)) = E ty (Var (i ++ [GoRight])) 
oSnd other   = error $ "oSnd failed" ++ show other

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
evalExprE e@(Lit v)		= return e
-- try some evaluation, please.
evalExprE (Choose a b c) = 
	case liftM unE $ evalE c of
	  Just (O_Bool True)  -> liftM unE $ evalE a
	  Just (O_Bool False) -> liftM unE $ evalE b
	  other -> Nothing
evalExprE (Alpha n e) =
	case liftM unE $ evalE e of
	  Just (O_RGB (RGB r g b)) -> return (O_RGBA (RGBA r g b n))
evalExprE other = Nothing

evalE :: E -> Maybe E
evalE (E t e) = liftM (E t) (evalExprE e)

------------------------------------------------------------------------------
-- Reification Support
------------------------------------------------------------------------------

-- The generic plubing for our Expr datatype.				-- 


-- TODO: allow Graph Expr to include the type
instance MuRef E where
  type DeRef E = Expr
  mapDeRef f (E _ e) = T.traverse f e



instance T.Traversable Expr where
	traverse f (Var i)		= pure $ Var i

	traverse f (O_Bool v)		= pure $ O_Bool v
	traverse f (O_RGB v)		= pure $ O_RGB v
	traverse f (O_RGBA v)		= pure $ O_RGBA v
	traverse f (Lit r)		= pure $ Lit r

--	traverse f (O_Fst a) 		= O_Fst <$> f a
--	traverse f (O_Snd a) 		= O_Snd <$> f a

	traverse f (Choose a b c) 	= Choose <$> f a <*> f b <*> f c
	traverse f (Mix a b c) 		= Mix <$> f a <*> f b <*> f c
	traverse f (Alpha c e) 		= Alpha c <$> f e
	traverse f (ScaleAlpha c e) 	= ScaleAlpha c <$> f e
	traverse f (UnAlpha e1 e2) 	= UnAlpha <$> f e1 <*> f e2

        traverse f (WithMask v1 v2)	= pure WithMask 	<*> f v1 <*> f v2
        traverse f (WithDefault v1 v2)	= pure WithDefault 	<*> f v1 <*> f v2
        traverse f (EQUAL v1 v2)	= pure EQUAL 		<*> f v1 <*> f v2
	-- TODO
	
instance F.Foldable Expr where
	foldMap f (Choose a b c) = mconcat [f a, f b, f c]
	--- TODO
	
instance Functor Expr where
	fmap f (Choose a b c) = Choose (f a) (f b) (f c)
	--- TODO
