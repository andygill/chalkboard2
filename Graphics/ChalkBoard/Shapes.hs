-- |
-- Module: Graphics.ChalkBoard.Shapes
-- Copyright: (c) 2009 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This module contains some basic shape generators, expressed as @Board Bool@.
--
-- TODO: Bad name for this module! Should be a Utils style module.

module Graphics.ChalkBoard.Shapes where

import Graphics.ChalkBoard.Board hiding (zip)
import Graphics.ChalkBoard.Shader
import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.O
import Graphics.ChalkBoard.Utils

-- import Control.Applicative

{-
-- | unit circle, radius 0.5, over origin.
circle :: Board Bool
circle =  Cond (InsideCircle (0,0) 0.5 1 0) (Pure True) (Pure False)
circle' = (\ (x,y) -> x*x + y*y <= 0.5 * 0.5) <$> coord

-- | unit vertical bar,  1 wide over origin.
vbar :: Board Bool
vbar =  (\ (_x,y) -> y <= 0.5 && y >= -0.5) <$> coord

-- | unit horizontal bar, 1 high over origin.
hbar :: Board Bool
hbar =  (\ (x,_y) -> x <= 0.5 && x >= -0.5) <$> coord

-- | unit square, 1x1 over origin.
square	 = Cond (InsideBox (-0.5,-0.5) 1 1 0) (Pure True) (Pure False)
square' = liftA2 (&&) vbar hbar
-}

{-
--squareO :: Board (O Bool)
-- squareO = Polygon [(-0.5,-0.5),(-0.5,0.5),(0.5,0.5),(0.5,-0.5)]

-- | cheacker board, with squares 1x1.
checker :: Board Bool
checker = (\ (x,y) -> even ((floor x + floor y) :: Int)) <$> coord

-- | Given two @Point@s, and a thickness, draw a line between the points.
-- line :: Line -> Double -> Board Bool

-}
{-
straightline' ((x1,y1),(x2,y2)) width = (\ (x,y) ->
---	distance (x1,y1) (x,y) <= width ||
--	distance (x2,y2) (x,y) <= width ||
	(  let 	u = intervalOnLine ((x1,y1),(x2,y2))  (x,y)
	   in u >= 0 
	   && u <= 1 
	   && distance (lerp (x1,y1) (x2,y2) u) (x,y) <= width
	)) <$> coord
-}

-- | A straight line, of a given width, between two points.

straightLine :: (Point,Point) -> R -> Board Bool
straightLine ((x1,y1),(x2,y2)) w = 
          move (x1,y1)
        $ rotate (pi /2 - th)
	$ box ((-w/2,0),(w/2,len))
  where
          (xd,yd)  = (x2 - x1,y2 - y1)
          (len,th) = toPolar (xd,yd)

pointsToLine :: [Point] -> R -> Board Bool
pointsToLine points width = stack
	[ straightLine (p1,p2) width
	| (p1,p2) <- zip points (tail points)
	] `over` stack 
	[ dotAt p width | p <- tail (init points) ]

-- | place dot at this location, with given diameter.
dotAt :: Point -> R -> Board Bool
dotAt p w = move p $ scale w circle
          
-- | A line generated by sampling a function from @R@ to @Point@s,
-- with a specific width. There needs to be at least 2 sample points.

functionLine :: (R -> Point) -> R -> Int -> Board Bool
functionLine line width steps = pointsToLine samples width
    where
	samples = map line (outerSteps steps)

-- | arrowhead is a triangle, pointing straight up, height 1, width 1, with the (0,0) at the center of the base.
--arrowhead :: Point -> Radian -> R -> Board Bool
--arrowhead p rad sz = move p $ rotate rad $ scale sz $ (\ (x,y) -> y >= 0 && y <= 1 && abs x * 2 <= 1 - y) <$> coord		

class LerpBoard a where
 lerpBoard :: Board a -> Board a -> Board UI -> Board a

instance LerpBoard RGB where
  lerpBoard b1 b2 bU = 
	gslBoard fn
		[ ("b1",ResultSize,board b1)
		, ("b2",ResultSize,board b2)
		, ("bU",ResultSize,board bU)
		]
		[]
    where 
      fn = unlines [
 	"uniform sampler2D b1;",
	"uniform sampler2D b2;",
	"uniform sampler2D bU;",
	"void main (void) {",
	" gl_FragColor.rgb = ",
	"   mix(",
	"    texture2D(b1,gl_TexCoord[0].st).rgb,",
	"    texture2D(b2,gl_TexCoord[0].st).rgb,",
	"    texture2D(bU,gl_TexCoord[0].st).r);",
        " gl_FragColor.a = 1.0;",
	"}" ]
	

instance (LerpBoard a) => Lerp (Board a) where
   lerp ui b1 b2  = lerpBoard b1 b2 (boardOf (o ui))

class ChooseBoard a where
 chooseBoard :: Board a -> Board a -> Board Bool -> Board a

-- anti-aliasing support
class SuperSample a where
  superSample :: Int -> Board a -> Board a


--class MulBoard a where
--  mulBoard :: Float -> Board a -> Board a

