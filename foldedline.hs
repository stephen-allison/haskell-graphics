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
  drawFoldedLine (0.3,-0.8) (186, 80) (color3f 1 0 1)
  --drawFoldedLine (-0.3,-0.1) (43, 75) (color3f 0 1 0)
  --drawFoldedLine (0.8,0.8) (90, 3) (color3f 1 1 0)
  flush
  --t <- getPOSIXTime
  --putStrLn $ show t

drawFoldedLine p1 p2 clr =  do
  let pts = foldedPoints simpleBox p1 p2
  let vertices =  clr : map (\(x,y) -> vertex3f x y 0) pts
  renderPrimitive LineStrip $ sequence_ vertices



color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
