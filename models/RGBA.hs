import Test.QuickCheck as QC

data RGBA = RGBA Float Float Float Float
	deriving (Show,Eq)

eq c1@(RGBA r g b a) c2@(RGBA r' g' b' a') 
		| a == 0 && a' == 0 = True
		| otherwise	    = c1 == c2


--     blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, OneMinusSrcAlpha)) -- Specify color and alpha blend separately
--
-- Order:
--
--glBlendFuncSeparateEXT(GLenum sfactorRGB,
--                             GLenum dfactorRGB,
--                             GLenum sfactorAlpha,
--                             GLenum dfactorAlpha)


merge :: RGBA -> RGBA -> RGBA
merge (RGBA r g b a) (RGBA r' g' b' a') 
	= RGBA (ff r r') (ff g g') (ff b b') (gg a a')
  where
	ff s d = s * a + d * (1 - a)	-- (SrcAlpha, OneMinusSrcAlpha)
	gg s d = s * 1 + d * (1 - a)	-- (One, OneMinusSrcAlpha)
	
nums = [0,0.2,0.6,0.8,1]

instance Arbitrary RGBA where
 arbitrary = do r <- elements nums
	        g <- elements nums
	        b <- elements nums
	        a <- elements [0,1] -- nums
		return $ RGBA r g b a
		
test1 r1 r2 r3 = label (show (r1,r2,r3)) $ ((r1 `merge ` r2) `merge` r3) == (r1 `merge` (r2 `merge` r3))
  where
	types = (r1 :: RGBA) 

test2 r1 = label (show (r1,r1 `merge` (RGBA 0 0 0 0))) $ r1 `eq` (r1 `merge` (RGBA 0 0 0 0))
  where
	types = (r1 :: RGBA) 


