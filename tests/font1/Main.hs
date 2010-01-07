{-# LANGUAGE TypeFamilies, FlexibleInstances  #-}
import Graphics.ChalkBoard.O
import Graphics.ChalkBoard
import Graphics.ChalkBoard.O.Internals
import qualified Graphics.ChalkBoard.Font as Font
import Numeric
import Data.Boolean
import Data.Char

{-


test choose = x (x . (x `ss` choose))

ss f g x = f . (g x)

{-
test2 :: a -> O (a -> a -> Bool -> a)
test2 x = x (x (x choose))
-}

test2 a b = pure choose <*> a <*> b

x :: (O a -> O b) -> O (a -> b)
x = undefined

test4 = x (x . (x .) . choose)

test5 = xdot (xdot xdot .) (choose)

choose4 :: O a -> O a -> O a -> O Int -> O a
choose4 = undefined

test6 f = xdot (xdot (xdot xdot .) .) choose4

test7 f = x ((xdot (xdot xdot .) .) choose4)

class F x where
    	type Fx x
	collect :: x -> Fx x

instance F (O a) where
	type Fx (O a) = O a

instance F b => F (O a -> b) where
	type Fx (O a -> b) = O (a -> Fx b)

xx :: (b -> O b1) -> (O a -> b) -> O (a -> b1)
xx f g = x (f . g)

xdot :: (a -> O a1 -> O b) -> a -> O (a1 -> b)
xdot f g = x (f  g)

xx2 :: (b -> O b1) -> (O a -> b) -> O (a -> b1)
xx2 f g = xdot (f .) g

mx :: (t -> t1 -> O a -> O b) -> t -> t1 -> O (a -> b)
mx c f g = x (f `c` g)

mx2 :: (t -> O a -> O b) -> t -> O (a -> b)
mx2 c f = x (c f)



--test3 choose = xdot (xdot (xdot choose))


instance Functor O where {}

instance Applicative O where {}


-}


main = do
     	font <- Font.initFont "../../Arial.ttf" 0
	startChalkBoard [BoardSize 400 400] $ \ cb -> main2 font cb 0.04


main2 font cb sz = do
	sp <- Font.lineSpacing font sz
	print sp
	(title,sp2) <- Font.label font sz ("ChalkBoard") -- Logistics of Moving Satellites in Space")
	print sp2

	
	

	{-	
	point1 <- Font.label font sz ([chr 0x25cf] ++ " KU supplied 4 ''binaries'', one for each problem.")
        point2 <- Font.label font sz ([chr 0x25cf] ++ " We also provided specification of small virtual machine, to run these binaries.")
        point3 <- Font.label font sz ([chr 0x25cf] ++ " Contestants write the VM, then interact with virtual actuators to fire rockets " ++
                                                                "and virtual sensors to detect location in orbit.")
        point4 <- Font.label font sz ([chr 0x25cf] ++ " Contestants upload an audit trail of what actuator fires when.")
        point5 <- Font.label font sz ([chr 0x25cf] ++ " We replay these on our local VM, validate the score, and update a leader board.")
        point6 <- Font.label font sz ([chr 0x25cf] ++ " At the end of the contest, teams upload their final source files.")
        point7 <- Font.label font sz ([chr 0x25cf] ++ " We further evaluate/validate the top 10 scoring entries to determine the winners.")

	-}
        
	drawChalkBoard cb (mix black white <$> (scaleXY (1,1) $ 
	                                        move (-0.45,0) $
	                                        scale (0.9 * (1/sp2)) $
	                                        title
	                                       )
	                  )
        return ()



{-
\begin{frame}[fragile]
\frametitle{Logistics of Moving Satellites in Space}
\begin{itemize}
\item KU supplied 4 ``binaries'', one for each problem.
\item We also provided specification of small virtual machine,
to run these binaries.
\item Contestants write the VM, and interact with virtual
actuators, to fire rockets, and virtual sensors, to detect location in orbit.
\item Contestants upload an audit trail of what actuator fires when.
\item We replay these on our local VM, and validate the score, and update a leader board.
\item At the end of the contest, teams upload their source files.
\item Only looking at top 10, to determine the winners.
\end{itemize}
\end{frame}
-}


