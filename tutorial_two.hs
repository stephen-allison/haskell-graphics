import Graphics.UI.GLUT

	-- define type as list of triples
myPoints :: [(GLfloat,GLfloat,GLfloat)]

	-- define points using list comprehension
myPoints = [ (sin(2*pi*k/12), cos(2*pi*k/12), 0) | k <- [1..12] ]

main :: IO ()
main = do
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "Hello World!"
	displayCallback $= display
	mainLoop

	-- DisplayCallback is defined as type DisplayCallback = IO ()
display :: DisplayCallback
display = do
	clear [ ColorBuffer ]
		
		-- renderPrimitive :: PrimitiveMode -> IO a -> IO a
		-- here PrimitiveMode is Points and the array of IO actions 
		-- 	is the result of the mapM_ call

		-- mapM_ is in Prelude and Control.Monad
		-- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
		-- mapM_ f is equivalent to sequence_ . map f

	renderPrimitive Points $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) myPoints
	flush