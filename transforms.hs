import Graphics.UI.GLUT
import Data.IORef

main :: IO ()
main = do
  (_progName, args) <- getArgsAndInitialize     --initialises opengl
  _window <- createWindow "Hello Open GL World"

  angle <-newIORef 0.0

  displayCallback $= display angle
  idleCallback $= Just (idle angle)

  mainLoop

display :: IORef GLfloat -> DisplayCallback
display angle = do
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
      vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

  clear [ColorBuffer]
  loadIdentity
  position (Light 0) $= Vertex4 0 50 (50) 1


  color3f 0 1 1
  a <- get angle
  rotate a $ Vector3 1 1 (0 :: GLfloat)
  renderObject Wireframe Icosahedron

  flush

idle :: IORef GLfloat -> IdleCallback
idle angle = do
  angle $~! (+ 0.1)
  postRedisplay Nothing
