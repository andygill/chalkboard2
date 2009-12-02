{-# LANGUAGE TypeFamilies, GADTs  #-}
module Graphics.ChalkBoard.O.Internals
	( O(..) 
	, primO
	, runO0
	, runO1
	, showO
	, reifyO
	, OType(..)
	, typeOfFun
	) where
	
import Graphics.ChalkBoard.Expr as Expr
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Reify.Graph
import Data.Reify
import Data.List as L
	
------------------------------------------------------------------------------------------------
-- Our O (Observable) structure.
------------------------------------------------------------------------------------------------

		
data O o where
   O :: o -> E -> O o
--   Lam :: (O a -> O b) -> O (a -> b)

-- Assuming that o is *not* a function, otherwise
-- <*> will fail with a pattern match failure.
primO :: Expr E -> o -> O o
primO e o = O o (E $ e)


runO1 :: (O a -> O b) -> E -> E
runO1 f v1 = case f (O (error "undefined shallow value") v1) of
	    O _ e -> e

runO0 :: O a -> E
runO0 (O _ e) = e

instance Show o => Show (O o) where
  show (O o _) = show o

-- showing structure, not the value
showO :: (Show a) => O a -> String
showO = undefined

reifyO :: O a -> IO (Graph Expr)
reifyO (O _ e) = reifyGraph e


data OType = UNKNOWN_TY | FUN_TY OType OType | EXPR_TY ExprType
	deriving Show

-- Here is the problem: given the result type, what is the argument type?
	
typeOfO :: O a -> OType
typeOfO (O a e) =
	case exprTypeE e of
 	  Nothing -> UNKNOWN_TY
	  Just ty  -> EXPR_TY ty
--typeOfO (Lam e) = typeOfO' 0 (Lam e)

typeOfO' :: Int -> O a -> OType
typeOfO' i o@(O {}) = typeOfO o

typeOfFun = typeOfFun' 0 


typeOfFun' i e = FUN_TY ty1 ty2
	   where
	     e' = (e (O (error "typeoOfO") (E $ Var i)))
  	     ty2 = typeOfO (e (O (error "typeoOfO") (E $ Var i)))
  	     ty1 = case L.lookup i (exprUnifyO e' ty2) of
	 	     Nothing -> error "opps: typeOfO"
		     Just ty -> ty
		
exprUnifyO :: (O a) -> OType -> [(Int,OType)]
exprUnifyO (O a e) (EXPR_TY ty) = [ (i,EXPR_TY t) | (i,t) <- exprUnifyE e ty ]
exprUnifyO (O a e) ty = error $ "exprUnifyO (O ...) " ++ show ty
--exprUnifyO (Lam e) (FUN_TY t1 t2) = []		--- for now
--exprUnifyO (Lam e) ty = error $ "exprUnifyO (Lam ...) " ++ show ty


