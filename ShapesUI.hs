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

  let tri = triangle (0,0.5) (0.5,0) (-0.5,0)
  let tri2 = triangle (0,0.75) (0.5,0.25) (-0.5,0.25)
  let col = color3f 1 1 0
  let s = merge tri tri2
  --let cuts1 = nub . mconcat $ cutLine <$> tri2 <*> tri
  --let cuts2 = nub . mconcat $ cutLine <$> tri <*> tri2
  --drawShape tri  (color3f 1 0 1)
  --drawShape tri2 (color3f 0 1 0)
  --drawShape cuts1 (color3f 1 0 0)
  --drawShape cuts2 (color3f 0 1 0)
  putStrLn $ show s
  drawShape s (color3f 0 1 0)
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
