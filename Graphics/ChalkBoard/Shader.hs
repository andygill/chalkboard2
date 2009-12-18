module Graphics.ChalkBoard.Shader where
	
import Graphics.ChalkBoard.Internals
import Graphics.ChalkBoard.Types

-- | gslBoard is mid-level API into the the GSL shader langauge.
gslBoard :: String -> [UniformArgument] -> [BoardArgument] -> Board a	
gslBoard = BoardGSI

