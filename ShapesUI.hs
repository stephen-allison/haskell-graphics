import Data.List
import Graphics.UI.GLUT
import Shapes

main :: IO ()
main = do
  (_progName, args) <- getArgsAndInitialize     --initialises opengl
  _window <- createWindow "World of Shapes"
  windowSize $= Size (600 :: GLsizei) (600 :: GLsizei)
  displayCallback $= display
  mainLoop



display = do
  clear [ColorBuffer]

  let col = color3f 1 1 0
  let t1 = triangle1
  let t2 = triangle2
  let t3 = triangle3
  let b1 = box1
  let s = perimeterLines [t3, t2, t1, b1]



  drawShape t1 (color3f 1 1 0)
  drawShape t2 (color3f 0 1 0)
  drawShape t3 (color3f 1 1 1)
  drawShape b1 (color3f 0 0 1)

  drawShape s (color3f 1 0 1)
  flush

drawShape s colour= do
  let points = rawPoints s
  let segs = map (\(x,y) -> vertex3f (glfloat x) (glfloat y) 0) points
  colour
  renderPrimitive Lines $ sequence_ segs

glfloat :: Real n => n -> GLfloat
glfloat x = realToFrac x
rawPoints segs = foldr (\(p1,p2) acc -> p2:p1:acc) [] segs
color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
