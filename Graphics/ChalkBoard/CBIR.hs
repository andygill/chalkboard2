{-# LANGUAGE FlexibleInstances #-}

module Graphics.ChalkBoard.CBIR where


import Graphics.ChalkBoard.Types (UI,RGB(..),RGBA(..))
import Graphics.ChalkBoard.Internals

import Data.Binary
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Monad

{-What do we need?

    * A way of creating a canvas
    * The canvas has size, pixels, depth, RGB/BW, opt alpha, perhaps 1-bit array.
    * We have ways of placing slices (rectangles?) of these canvases onto the 'screen'. 

        Difference places, sizes, rotations.

    * Ways drawing these to a canvas
    * Ways of importing and exporting canvases 
-}


type BufferId       = Int		     -- make abstact later
type StreamId       = Int
type FragFunctionId = Int


data Depth = BitDepth		-- 1 bit per pixel
           | G8BitDepth        -- 8 bits per pixel (grey)
           | RGB24Depth	        -- (R,G,B), 8 bits per pixel
           | RGBADepth		-- (R,G,B,A), 8 bits per pixel
        deriving (Show, Eq)

-- In a sense, the Depth is the type of our CBBO.

-- KM: I would say leave as 0-1 since that's what OpenGL defaults to anyway for colors. Plus then if you want to use it for something else like you did with PointMap?, you can.

data Background
           = BackgroundBit Bool
{-
AG: does a pixel mean later 'draw this *color* (white/black), or draw this pixel *if* black?

KM: I would say the "draw this pixel if true" approach might be faster? Then could use the

    8Bit version if you want to force it to draw all pixels?
-}
           | BackgroundG8Bit UI
              -- this may have the same issue?
           | BackgroundRGB24Depth RGB
           | BackgroundRGBADepth RGBA
	   | BackgroundByteString ByteString 

instance Show Background where
	show (BackgroundBit b) = "(BackgroundBit $ " ++ show b ++ ")"
	show (BackgroundG8Bit g) = "(BackgroundG8Bit $ " ++ show g ++ ")"
	show (BackgroundRGB24Depth c) = "(BackgroundRGB24Depth $ " ++ show c ++ ")"
	show (BackgroundRGBADepth c) = "(BackgroundRGBADepth $ " ++ show c ++ ")"
	show (BackgroundByteString _) = "(BackgroundByteString ...)"
	

instance Show (a -> IO()) where
        show _ = "(Callback Function)"


-- type RGBA = (UI,UI,UI,UI)

type UIPoint = (UI,UI)

-- A mapping from a point on the source CBBO to a corresponding point on the canvas CBBO.
data PointMap = PointMap UIPoint UIPoint
        deriving Show

-- Telling CopyBoard whether to use the source alpha or the destination alpha (use source for a /complete/ copy)
data WithAlpha = WithSrcAlpha
               | WithDestAlpha
        deriving Show

-- Ways of blending the background with the new splat-ee.
-- Blend ==> Alpha Blend
-- Sum   ==> Add back and splat-ee (what about alpha? set it to one?)
-- Max   ==> Take max of back and splat-ee (again, what about alpha)
-- Copy  ==> use the splat-ee only,

data Blender = Blend | Sum | Max | Copy deriving (Eq, Show)

-- AG: The depth is determined by the Background, we only need one!

-- We now use Inst var, but use Inst CBBO in most cases.
data Inst var
     = Allocate 
        var             -- tag for this ChalkBoardBufferObject
        (Int,Int)       -- size of ChalkBoardBufferObject
        Depth           -- depth of buffer
        Background      -- what to draw at allocation

       -- ^ This means allocate a buffer, n * m pixels, with a specific depth, and a background (default) color.
{-
AG: other considerations include

    * Do we project into it using 0..1 x 0..1? (say yes for now) 

        (KM: Sounds fine as a first approach. Could support both exact position and percentage (0-1) eventually.)

    * Does it loop round, when viewed? 

        (KM: I would say not when printed onto the screen, at least. Maybe internally for 'from' CBBOs doing splats, but the 'to' board probably won't want it to wrap, just be off the edge.)
-}


     | Splat var Blender (Splat var)

{-
     | SplatWholeBoardColor
	RGBA
	var
-}

     | AllocateImage
       var
       FilePath
     
     | SaveImage
       var
       FilePath
       
     | OpenStream
       StreamId
       String
       Bool		-- if you want the stream status verbose
     
     | WriteStream
       BufferId
       StreamId
     
     | CloseStream
       StreamId
       
     | Delete
       var
       
--     | ScaleAlpha var UI	-- LATER


    | Nested String [Inst var]

    | Exit

	-- GLSL extensions to CBIR

    | AllocFragmentShader var String [String]	-- GLSL object, and names of args

    | ShadeFragmentWith var [(String,var)] [Inst var]	-- Use a specific object, in the context of the given framement function

    | DeleteFragmentShader var		-- TODO: write code for, and use
    
    
    | ChangeMouseCallback (UIPoint -> IO())     -- TODO: binary instance
    
    | ChangeKeyboardCallback (Char -> IO())     -- TODO: binary instance

        deriving Show


data Splat var
	= SplatPolygon' 
		var 			-- source board
		[PointMap]		-- points
	| SplatColor' 
		RGBA
		[UIPoint]
	| SplatFunction'
       		var			-- FragFunId
       		[(String,var)]		-- argument BufferId(s)
       		[(String,UniformArgument)]		-- the extra uniform args
       		[UIPoint]		-- should be UIPoint???
	| SplatBuffer'
		var			-- source
	  -- todo, consider also CopyBuffer
	deriving (Show)

copyBoard :: var -> var -> Inst var
copyBoard src target = Splat target Copy (SplatBuffer' src)

colorBoard :: RGB -> var -> Inst var
colorBoard (RGB r g b) target = Splat target Copy (SplatColor' (RGBA r g b 1) [ p | p <- [(0,0),(0,1),(1,1),(1,0)]])

{-

AG: Questions and considerations

    * Do you average each same point, inside the triangle from its neighbours, or just sample 1? (We'll test both, and see how they look.) 

        KM: Maybe I'm interpreting the question wrong, but I would think you just take each _vertex_ color

            (1 point sample) and then have OpenGL do blending later? (texture maps would be special splats)

        AG: I think we'll want both. I think of splat as cutting out a piece of the source CBBO, and pasting it onto the dest CBBO.

    * How do we introduce alpha, here? Is it always a property of the source CBBO, or it there other places we can determine this. 

        KM: I would say source alpha. If you want something to cover completely, you leave source alpha as one. If you want it to be partly transparent, you change the source alpha beforehand?

    * Also, we should still support rectangles primitively? 

        KM: Right, and we technically only need both corners as input for this (you mean rectangle splatting right?) Might also want primitive support for lines at least, and since it's designed for presentations, we could have primitive circles/ellipses/text/(arrows?) as well if we want. At least, I think that calculating things like ellipse->triangles would probably be faster in OpenGL/C if we can get it pushed down that far.
-}

{-

Other Considerations ¶

    * How do we allocate (or load from a structure) a pre-existing image? Is this done at create time only, or do we want to update a CBBO? 

    * I'm not sure about the defaults, vs the loading of an 'image'. Both are sorts of assignment/update, as is loading from a file. 

   example1 = alloc (2,2) Bit [[ 0, 1 ], [ 1, 0 ]]
   example2 = alloc (2,2) Bit [[0]]
   example3 = alloc (2,2) Bit "foo.ppm"

-}


showCBIRs :: Show i => [Inst i] -> String
showCBIRs insts = concat [ prespace " " ([ch] ++ " " ++ show' inst) | (ch,inst) <- zip ('[':repeat ',') insts ] ++ " ]\n"
 where
  show' (Nested msg [])     = "Nested " ++ show msg ++ "[]"
  show' (Nested msg insts') = "Nested " ++ show msg ++ "\n" ++ prespace "   " (showCBIRs insts')
  show' other		= show other
  prespace c		= unlines . map (\ m -> c ++ m) . lines

instance Binary PointMap where
  put (PointMap a b)  = put a >> put b
  get = liftM2 PointMap get get

instance Binary WithAlpha where
  put WithSrcAlpha = put (0 :: Word8)
  put WithDestAlpha = put (1 :: Word8)
  get = do tag <- getWord8
           case tag of
                  0 -> return $ WithSrcAlpha
                  1 -> return $ WithDestAlpha

instance Binary Blender where
  put Blend     = put (0 :: Word8)
  put Sum       = put (1 :: Word8)
  put Max       = put (2 :: Word8)
  put Copy      = put (3 :: Word8)
  get = do tag <- getWord8
           case tag of
                  0 -> return $ Blend
                  1 -> return $ Sum
                  2 -> return $ Max
                  3 -> return $ Copy

instance Binary Depth where
  put BitDepth 	  = put (0 :: Word8)
  put G8BitDepth  = put (1 :: Word8)
  put RGB24Depth  = put (2 :: Word8)
  put RGBADepth   = put (3 :: Word8)
  get = do tag <- getWord8
           case tag of
                  0 -> return $ BitDepth
                  1 -> return $ G8BitDepth
                  2 -> return $ RGB24Depth
                  3 -> return $ RGBADepth

{-
data Background
           = BackgroundBit Bool
{-
AG: does a pixel mean later 'draw this *color* (white/black), or draw this pixel *if* black?

KM: I would say the "draw this pixel if true" approach might be faster? Then could use the

    8Bit version if you want to force it to draw all pixels?
-}
           | BackgroundG8Bit UI
              -- this may have the same issue?
           | BackgroundRGB24Depth UI UI UI
           | BackgroundRGBADepth UI UI UI UI
           | BackgroundPtr (Ptr CUChar)		-- the way to interpretate this depends on the Depth field.
	   | BackgroundArr (IStorableArray (Int,Int,Int))
	-}

instance Binary UniformArgument where
  put (UniformArgument arg) 		= put (0 :: Word8) >> put arg 
  put (UniformListArgument args) 	= put (1 :: Word8) >> put args 
  
  get = do tag <- getWord8
	   case tag of
		0 -> liftM UniformArgument get
		1 -> liftM UniformListArgument get


instance Binary Argument where
  put (Int v) 		= put (0 :: Word8) >> put v
  put (Float v) 	= put (1 :: Word8) >> put v
  put (Vec2 v) 		= put (2 :: Word8) >> put v
  put (Arr v) 		= put (3 :: Word8) >> put v
  put (ArrVec2 v) 	= put (4 :: Word8) >> put v
  put (Vec3 v) 		= put (5 :: Word8) >> put v
  put (Vec4 v) 		= put (6 :: Word8) >> put v

  get = do tag <- getWord8
           case tag of
		0 -> liftM Int get
		1 -> liftM Float get
		2 -> liftM Vec2 get
		3 -> liftM Arr get
		4 -> liftM ArrVec2 get
		5 -> liftM Vec3 get
		6 -> liftM Vec4 get
		
instance Binary Background where
  put (BackgroundBit b) 	 = put (0 :: Word8) >> put b
  put (BackgroundG8Bit g) 	 = put (1 :: Word8) >> put g
  put (BackgroundRGB24Depth rgb) = put (2 :: Word8) >> put rgb
  put (BackgroundRGBADepth rgba) = put (3 :: Word8) >> put rgba
  put (BackgroundByteString arr) = put (4 :: Word8) >> put arr
  get = do tag <- getWord8
           case tag of
                  0 -> liftM BackgroundBit get
                  1 -> liftM BackgroundG8Bit get
                  2 -> liftM BackgroundRGB24Depth get
                  3 -> liftM BackgroundRGBADepth get
                  4 -> liftM BackgroundByteString get


instance (Show var, Binary var) => Binary (Splat var) where
  put (SplatPolygon' v ps)              = put (0 :: Word8) >> put v >> put ps
  put (SplatColor' rgba ps)             = put (1 :: Word8) >> put rgba >> put ps
  put (SplatFunction' v bargs uargs ps) = put (2 :: Word8) >> put v >> put bargs >> put uargs >> put ps
  put (SplatBuffer' v)                  = put (3 :: Word8) >> put v
  get = do tag <- getWord8
           case tag of
                0 -> liftM2 SplatPolygon' get get
                1 -> liftM2 SplatColor' get get
                2 -> liftM4 SplatFunction' get get get get
                3 -> liftM  SplatBuffer' get


instance (Show var, Binary var) => Binary (Inst var) where
  put (Allocate v sz d b) 	= put (0 :: Word8) >> put v >> put sz >> put d >> put b
  put (Splat v blend stype)     = put (1 :: Word8) >> put v >> put blend >> put stype
  put (AllocateImage _ _) 	= error "AllocateImage"
  put (SaveImage v nm)		= put (2 :: Word8) >> put v >> put nm
  put (OpenStream sid str verb) = put (3 :: Word8) >> put sid >> put str >> put verb
  put (WriteStream bid sid)     = put (4 :: Word8) >> put bid >> put sid
  put (CloseStream sid)         = put (5 :: Word8) >> put sid
  put (Delete v)		= put (6 :: Word8) >> put v 
  put (Nested nm insts)		= put (7 :: Word8) >> put nm >> put insts
  put (Exit)			= put (8 :: Word8)
  put (AllocFragmentShader v txt args)  = put (9 :: Word8) >> put v >> put txt >> put args
  put (ShadeFragmentWith v args insts)  = put (10 :: Word8) >> put v >> put args >> put insts
  put (DeleteFragmentShader v)  = put (11 :: Word8) >> put v
  --put other			= error $ show ("put",other)

  get = do tag <- getWord8
	   case tag of
		0 -> liftM4 Allocate get get get get
		1 -> liftM3 Splat get get get
		2 -> liftM2 SaveImage get get
		3 -> liftM3 OpenStream get get get
		4 -> liftM2 WriteStream get get
		5 -> liftM  CloseStream get
		6 -> liftM  Delete get
		7 -> liftM2 Nested get get
		8 -> return $ Exit
		9 -> liftM3 AllocFragmentShader get get get
		10 -> liftM3 ShadeFragmentWith get get get
		11 -> liftM  DeleteFragmentShader get

