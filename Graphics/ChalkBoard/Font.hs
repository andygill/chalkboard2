module Graphics.ChalkBoard.Font
	( Graphics.ChalkBoard.Font.initFont
	, Font
	, letter
	, lineSpacing
	, label
	) where


import Data.Boolean

import Data.Ix
import Data.Array.Unboxed
import Graphics.Rendering.TrueType.STB hiding (Font)
import qualified Graphics.Rendering.TrueType.STB as STB
--import Graphics.ChalkBoard
import Graphics.ChalkBoard.Utils
import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.Board
import Graphics.ChalkBoard.Buffer
import Graphics.ChalkBoard.O

import qualified Data.ByteString as BS

data Font = Font
	STB.Font		-- the font
	()			-- the cache for each use char

initFont :: String -> Int -> IO Font
initFont fontFile ix = do
    tt <- loadTTF fontFile
    en <- enumerateFonts tt
    font <- STB.initFont tt (en !! ix)
    return $ Font font ()


lineSpacing :: Font -> Float -> IO Float
lineSpacing (Font font _) sz = do
	met <- getFontVerticalMetrics font
	return $ sz * (fromIntegral (ascent met - descent met + lineGap met))
	

label :: Font -> Float -> String -> IO (Board UI, Float)
label font sz str = do
	
	let brd0 :: Board (UI)
	    brd0 = boardOf 0.0 --flip withMask false <$> boardOf 0.9

	brds <- sequence [ do
		(b,off) <- letter font sz ch
		return (b,off)
		| ch <- str
		]

	let lens :: [Float]
	    lens = 0 : Prelude.zipWith (+) (map snd brds) lens
	let brd1 :: Board UI
	    brd1 = foldr (\ (buff,off) brd -> buff `bufferOnBoard` (move (off,0) brd)) brd0 
			(Prelude.zip (map fst brds) (map id (map snd brds)))

	-- Use UI rather than Maybe UI later: it will be more efficient.
	-- because we avoid the big <$> here, over the *whole* board.
	return (brd1, sum (map snd brds))

letter :: Font -> Float -> Char -> IO 
	( Buffer UI		-- 
	, Float			-- how far to push rest of word to right
	)
letter (Font font ()) sz ch = do
    Just glyph_K <- findGlyph font ch
    bb_K <- getGlyphBoundingBox font glyph_K
    (bm_K,bo_K) <- newGlyphBitmap font glyph_K (sz,sz)
    m_K <- getGlyphHorizontalMetrics font glyph_K
    bma_K <- bitmapArray bm_K 

    -- The backing board must be a power of two (PoT).
    let pot' n x = if n > x then n else pot' (n * 2) x
    let pot = pot' 1

    let ((x0,y0),(x1,y1)) = bounds bma_K
--    print (x1,y1)

    let x1' = pot x1 - 1
    let y1' = pot y1 - 1

    let bs' = BS.pack [ if x > x1 || y > y1 then 0 else bma_K ! (x1 - (x - x0),y)
		      | x <- [x0..x1']
		      , y <- [y0..y1']
		      ]

    let x1' = x1 
    let y1' = y1
    let (x1'',y1'') = (pot x1' - 1,pot y1' - 1)

    let (bo_x,bo_y) = bo_K

    let (BBox (a,b) (c,d)) = bb_K
    xx <- getGlyphBitmapBox font glyph_K (sz,sz)
    let (BBox (a,b) (c,d)) = xx

    return 
	( moveBuffer (0 + ceiling (sz * fromIntegral (leftSideBearing m_K)),-d) $ newBufferUI bs' (y1''+1,x1''+1)
	, sz * fromIntegral (advanceWidth m_K) -- + the remainer from the ceiling operation
	)

just :: O UI -> O (Maybe UI)
just o = withMask o (o /=* 0)
