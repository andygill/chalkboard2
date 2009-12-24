{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances  #-}
module Graphics.ChalkBoard.O.Internals
	( O(..) 
	, primO
	, runO0
	, runO1
	, showO
	, reifyO
	, argTypeForOFun
	) where
	
import Graphics.ChalkBoard.Expr as Expr
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Reify.Graph
import Data.Reify
import Data.List as L
import Debug.Trace
	
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


instance Show o => Show (O o) where
  show (O o _) = show o

-- showing structure, not the value
showO :: (Show a) => O a -> String
showO (O a e) = show e

reifyO :: O a -> IO (Graph Expr)
reifyO (O _ e) = reifyGraph e

runO0 :: O a -> E
runO0 (O _ e) = e

runO1 :: (O a -> O b) -> E -> E
runO1 f v1 = case f (O (error "undefined shallow value") v1) of
	    O _ e -> e

-- Given a function, and the *result* type, give the argument type.
argTypeForOFun :: (O a -> O b) -> ExprType -> Maybe ExprType
argTypeForOFun f ty = L.lookup 0 (exprUnifyE e ty)
	 where
		(O _ e) = (f (O (error "typeOfO") (E $ Var 0)))
