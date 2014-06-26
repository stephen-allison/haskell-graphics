-- http://www.haskell.org/haskellwiki/OpenGLTutorial1

import Graphics.UI.GLUT

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

display :: DisplayCallback
display = do
	clear [ ColorBuffer ]
	flush

closer :: Maybe CloseCallback
closer = do
	Just $ print "closing" 	-- note that $ is needed because 
							-- of left associativity of fn application and
							-- its high precedence


