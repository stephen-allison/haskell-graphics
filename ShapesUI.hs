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

  let cut12 = cutShapeWithShape triangle1 triangle0
  let cut21 = cutShapeWithShape triangle0 triangle1
  let ex12 = excludeContained triangle0 cut12
  let ex21 = excludeContained triangle1 cut21

  print "--ex12--"
  print ex12
  print "--ex21--"
  print ex21
  let s = fuse ex12 ex21
{-
  let cut3s = cutShapeWithShape triangle3 s
  --print cut3s
  let cuts3 = cutShapeWithShape s triangle3
  --print cuts3
  let ex3s = excludeContained s cut3s
  let exs3 = excludeContained triangle3 s
  --print ex3s
  --print exs3

  let s2 = fuse ex3s exs3
  print s2
-}
  --drawShape triangle2 (color3f 0 1 0)
  drawShape s (color3f 1 1 0)
  --drawShape ex3 (color3f 1 0 1)
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
