module Graphics.ChalkBoard.Shader
	( gslBoard
	, UniformArgument(..)
	, Argument(..)
	, board
	, uniform
	) where
	
import Graphics.ChalkBoard.Internals
import Graphics.ChalkBoard.Types

-- We may need (smart) constructors for the UA's below, so that we can transmit this over the wire to the server.

-- | gslBoard is mid-level API into the the GSL shader langauge.
gslBoard :: String -> [(String,UniformTexture)] -> [(String,UniformArgument)] -> Board a	
gslBoard = BoardGSI

