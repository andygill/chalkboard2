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
     	let (w,h) = (800,600)
	startChalkBoard [BoardSize w h] $ \ cb -> main2 cb font 0.03 (w,h)


main2 cb font sz (w,h) = do
	sp <- Font.lineSpacing font sz
	--print sp
	(title,titleSP) <- Font.label font sz ("Logistics of Moving Satellites in Space")
	--print titleSP
	
		
	(point1, sp1) <- Font.label font (0.5*sz) ([chr 0x25cf] ++ " KU supplied 4 ''binaries'', one for each problem.")
        (point2, sp2) <- Font.label font (0.5*sz) ([chr 0x25cf] ++ " We also provided specification of small virtual machine, to run these binaries.")
        (point3, sp3) <- Font.label font (0.5*sz) ([chr 0x25cf] ++ " Contestants write the VM, then interact with the virtual actuators and sensors.")
                                                             --" Contestants write the VM, then interact with virtual actuators to fire rockets " ++
                                                             --  "and virtual sensors to detect location in orbit.")
        (point4, sp4) <- Font.label font (0.5*sz) ([chr 0x25cf] ++ " Contestants upload an audit trail of what actuator fires when.")
        (point5, sp5) <- Font.label font (0.5*sz) ([chr 0x25cf] ++ " We replay these on our local VM, validate the score, and update a leader board.")
        (point6, sp6) <- Font.label font (0.5*sz) ([chr 0x25cf] ++ " At the end of the contest, teams upload their final source files.")
        (point7, sp7) <- Font.label font (0.5*sz) ([chr 0x25cf] ++ " We further evaluate and validate the top 10 scoring entries to determine the winners.")
	--}
        
        --let over2 a b = over b a
        
        let slide = mix black white <$> (
	                                  (makeTitle (w,h) titleSP title)
	                                  `over`
	                                  (makeBullets (w,h) sp sp1 [point1, point2, point3, point4, point5, point6, point7])
	                                )
              
        let scaling = (fromIntegral h) / (fromIntegral w)
            scaledSlide = if (w > h)
                              then scaleXY (scaling,1) $ slide
                              else scaleXY (1,1.0/scaling) $ slide
        
	drawChalkBoard cb scaledSlide


        




makeTitle :: (Int,Int) -> Float -> Board UI -> Board UI
makeTitle (wboard,hboard) size lbl =  scale (0.9) $ move (-0.5015,0.45) $ scale (1/size) $ lbl

                                       
makeBullets :: (Int,Int) -> Float -> Float -> [Board UI] -> Board UI
makeBullets (w,h) sp size lbls = makeBullets_ (w,h) sp size lbls 0

makeBullets_ :: (Int,Int) -> Float -> Float -> [Board UI] -> Int -> Board UI
makeBullets_ (_,_) _ _ [] _ = (boardOf 0)
makeBullets_ (w,h) sp size (lbl:lbls) lines = (makeBullet (w,h) sp size lines lbl) `over` (makeBullets_ (w,h) sp size lbls (lines+1))

makeBullet :: (Int,Int) -> Float -> Float -> Int -> Board UI -> Board UI
makeBullet (w,h) sp size lines lbl = if (w > h)
                                         then move (-(1-fontSize)/2-(1-scaling)/2,0) $ scale (fontSize) $ move (-0.5015,spacing) $ scale (1/size) $ lbl
                                         else move (-(1-fontSize)/2,0) $ scale (fontSize) $ move (-0.5015,spacing) $ scale (1/size) $ lbl
                                     
                                     where  fontSize = 0.6
                                            spacing = 0.25 - (fromIntegral lines)*perLine
                                            perLine = (fromIntegral h)/sp*6/(fromIntegral h)--(sp)/(fromIntegral h)
                                            scaling = (fromIntegral h) / (fromIntegral w)
                                           



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


