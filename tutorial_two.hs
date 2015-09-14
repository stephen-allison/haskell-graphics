import Graphics.UI.GLUT

	-- define type as list of triples
myPoints :: [(GLfloat,GLfloat,GLfloat)]

	-- define points using list comprehension
myPoints = [ (0.8*sin(2*pi*k/12), 0.8*cos(2*pi*k/12), 0) | k <- [1..12] ]

main :: IO ()
main = do
	(_progName, args) <- getArgsAndInitialize 		--initialises opengl
	_window <- createWindow "Hello Open GL World"
		--what does $= mean?
		--seems to mean assignment to a global state var from
		--open gl StateVar module
	displayCallback $= display
	--closeCallback $= closer -- compiles but fails at runtime if this is in here
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

	renderPrimitive LineLoop $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) myPoints
	flush
