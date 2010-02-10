{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances  #-}
module Graphics.ChalkBoard.O.Internals
	( O(..) 
	, primO
	, runO0
	, runO1
	, showO
	, reifyO
	, typeO
	, typeO1
	, argTypeForOFun
	) where
	
import Graphics.ChalkBoard.Expr as Expr
--import qualified Data.Traversable as T
--import qualified Data.Foldable as F
--import Data.Reify.Graph
import Data.Reify
--import Data.List as L
--import Debug.Trace
import Graphics.ChalkBoard.Types  as Ty
import Data.Ratio
	
------------------------------------------------------------------------------------------------
-- Our O (Observable) structure.
------------------------------------------------------------------------------------------------

		
data O o where
   O :: o -> E -> O o
--   Lam :: (O a -> O b) -> O (a -> b)

-- Assuming that o is *not* a function, otherwise
-- <*> will fail with a pattern match failure.
primO :: E -> o -> O o
primO e o = O o e

instance Show o => Show (O o) where
  show (O o _) = show o

-- showing structure, not the value
showO :: O a -> String
showO (O _ e) = show e

reifyO :: O a -> IO (Graph Expr)
reifyO (O _ e) = reifyGraph e

runO0 :: O a -> E
runO0 (O _ e) = e

runO1 :: (O a -> O b) -> E -> E
runO1 f v1 = case f (O (error "undefined shallow value") v1) of
	    O _ e -> e

typeO :: O a -> ExprType
typeO (O _ e) = typeE e

typeO1 :: (O a -> O b) -> ExprType -> ExprType
typeO1 f ty1 = ty2
  where (O _ (E ty2 _)) = f (O (error "typeO1 (should not be looking here!)") (E ty1 $ Var []))

-- Given a function, and the *argument* (a) type, give types of the paths inside the argument.

argTypeForOFun :: (O a -> O b) -> ExprType -> [([Path],ExprType)]
argTypeForOFun f _ = error "(exprUnifyE e ty)"
	 where
		(O _ _) = (f (O (error "typeOfO (should not be looking here!)") (E (error "type unknown") $ Var [])))

instance Eq (O UI) where
	(O a _) == (O b _) = a == b

instance Num (O UI) where
{-
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
-}
  fromInteger i = O v (E UI_Ty $ Lit v)
	   where v = fromInteger i
	
instance Fractional (O UI) where
	fromRational r = O v (E UI_Ty $ Lit v)
	   where v = fromInteger (numerator r) / fromInteger (denominator r)
{-
	(/) :: a -> a -> a	
  	recip :: a -> a
-}

