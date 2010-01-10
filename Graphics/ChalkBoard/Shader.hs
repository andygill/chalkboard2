{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.ChalkBoard.Shader
	( gslBoard
	, UniformArgument(..)
	, Argument(..)
	, TextureSize(..)
	, board
	, buffer
	, uniform
	) where
	
import Graphics.ChalkBoard.Internals
import Graphics.ChalkBoard.Types
import Graphics.ChalkBoard.O.Internals
import Graphics.ChalkBoard.O

-- We may need (smart) constructors for the UA's below, so that we can transmit this over the wire to the server.

-- | gslBoard is mid-level API into the the GSL shader langauge.
gslBoard :: forall a . (Obs a) => String -> [(String,TextureSize,UniformTexture)] -> [(String,UniformArgument)] -> Board a	
gslBoard fn as1 as2 = Board (typeO (o (error "gslBoard" :: a))) (BoardGSI fn as1 as2)

