import Graphics.UI.GLUT
import Data.Time.Clock.POSIX

import BoxGeometry

main :: IO ()
main = do
  (_progName, args) <- getArgsAndInitialize     --initialises opengl
  _window <- createWindow "Hello Open GL World"
  windowSize $= Size (600 :: GLsizei) (600 :: GLsizei)
    --what does $= mean?
    --seems to mean assignment to a global state var from
    --open gl StateVar module
  displayCallback $= display
  --closeCallback $= closer -- compiles but fails at runtime if this is in here
  mainLoop

display = do
  clear [ColorBuffer]
  drawFoldedLine
  flush
  --t <- getPOSIXTime
  --putStrLn $ show t

drawFoldedLine =  do
  let pts = points simpleBox (0.3,-0.8) (93, 40.2)
  let vertices = (color3f 1 0 1) : map (\(x,y) -> vertex3f x y 0) pts
  putStrLn $ show pts
  renderPrimitive LineStrip $ sequence_ vertices





drawShip :: IO ()
drawShip = do
  renderPrimitive Triangles $ do
    color3f 1 0 0
    vertex3f 0 0 0
    vertex3f (-0.04) (-0.1) 0
    vertex3f 0 (-0.08) 0

    color3f 0 1 0
    vertex3f 0 0 0
    vertex3f 0.04 (-0.1) 0
    vertex3f 0 (-0.08) 0

color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
