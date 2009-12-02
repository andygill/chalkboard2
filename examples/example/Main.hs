import Graphics.ChalkBoard

main :: IO ()
main = startChalkBoard [] $ \ cb -> loop cb 0 (cycle colors)

loop :: ChalkBoard -> Float -> [O RGB] -> IO ()
loop cb n rgb@(c1:c2:c3:_) | n >= 0.999 = loop cb (n - 1) (tail rgb)
              | otherwise = do
  drawChalkBoard cb (choose (o (lerp n (unO c3) (unO c2))) c1 <$> rotate (n * pi) (scale (n * 1.1) square))
  loop cb (n + 0.01) rgb

colors :: [O RGB]
colors = [red,green,blue,yellow,cyan,purple,black,white]
