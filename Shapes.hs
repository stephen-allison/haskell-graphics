module Shapes where
import Data.List
import Data.Maybe

type Point = (Float, Float)
type LineSeg = (Point, Point)
type Box = (Point, Point, Point, Point)
type Shape = [LineSeg]
type TaggedSeg = (LineSeg, Bool)
type TaggedShape = [TaggedSeg]

lineSeg :: Float -> Float -> Float -> Float -> LineSeg
lineSeg x1 y1 x2 y2 = ((x1, y1), (x2, y2))



point :: Float -> Float -> Point
point = (,)



triangle :: Point -> Point -> Point -> Shape
triangle p1 p2 p3 = [(p1,p2), (p2,p3), (p3,p1)]


{-
(+) <$> [1,2,3] <*> [70,80,90] = [71,81,91,72,82,92,73,83,93]
-}
--join :: [Shape] -> TaggedShape
perimeterLines shapes = concat linesRemoved
     where allSegs = concat shapes
           cutShapes = zip shapes (map cutter shapes)
           cutter = (flip cutShapeWithShape) allSegs
           removeLines lines = foldl lineRemover lines
           lineRemover lines shape = excludeContained shape lines
           linesRemoved = map (\(shape,lines) -> removeLines lines (shapes \\ [shape] )) cutShapes



pointsToLineSegs :: [Point] -> [LineSeg]
pointsToLineSegs ps = zip ps (tail ps)



cutShapeWithShape :: Shape -> Shape -> Shape
cutShapeWithShape toCut cutter = concat $ map (cutLineWithShape cutter) toCut



cutLineWithShape :: Shape -> LineSeg -> [LineSeg]
cutLineWithShape cutter (a,b) = pointsToLineSegs sorted
     where cutpoints = catMaybes $ crossing <$> [(a,b)] <*> cutter
           sorted = sortOn (dist $ a) (a : b : cutpoints)


lineSegInShape :: Shape -> LineSeg -> Bool
lineSegInShape s (p1, p2) = pointInShape p1 s && pointInShape p2 s



excludeContained :: Shape -> [LineSeg] -> [LineSeg]
excludeContained contr = filter (\s -> not $ lineSegInShape contr s)



dist :: Point -> Point -> Float
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)**2 + (y2-y1)**2)



pointsFromShape :: Shape -> [Point]
pointsFromShape = foldr (\(p1,p2) acc -> p1:p2:acc) []



crossing :: LineSeg -> LineSeg -> Maybe Point
crossing seg1 seg2
     | slope seg1 == slope seg2 = Nothing
     | slope seg1 == inf = inbounds (getx p1, (gety p3) + (slope seg2) * (getx p3 - getx p1))
     | slope seg2 == inf = crossing seg2 seg1
     | otherwise = inbounds (fix x, fix y)
     where
     (p1, p2) = seg1
     (x1, y1) = p1
     (x2, y2) = p2
     (p3, p4) = seg2
     (x1', y1') = p3
     (x2', y2') = p4
     m = slope seg1
     m' = slope seg2
     x = (y1 - y1' + m'*x1' - m*x1) / (m' - m)
     y = y1 + m*(x - x1)
     inbounds p = if (onLine p seg1) && (onLine p seg2) then Just p else Nothing



pointInShape :: Point -> Shape -> Bool
pointInShape = flip contains



{-
lovely simple method based on cross product, from SoE book
AxB = ax.by - bx.ay = |A||B| sin Q
LHS >= 0 ==> Q in [0,180)
LHS < 0  ==> Q in [180,360)
-}
isLeftOf :: Point -> LineSeg -> Bool
(px, py) `isLeftOf` ((ax,ay), (bx,by))
     = let (s,t) = (px - ax, py - ay)
           (u,v) = (px - bx, py - by)
       in s*v >= t*u



contains :: Shape -> Point -> Bool
contains segs p = and $ map (leftOfOrOn p) segs
     where
     leftOfOrOn p seg = isLeftOf p seg || onLine p seg



slope :: LineSeg -> Float
slope (p1, p2)
     | dx == 0 = inf
     | otherwise = (gety p2 - gety p1) / dx
     where
     dx = (getx p2 - getx p1)



onLine :: Point -> LineSeg -> Bool
onLine p (a,b) =  bounded
     where
     m1 = fix $ slope (a,b)
     m2 = fix $ slope (a,p)
     bounded = pointInBoundingBox p (a,b)



pointInBoundingBox :: Point -> LineSeg -> Bool
pointInBoundingBox p (p1, p2) = xok && yok
     where
     xok = (getx p <= xmax) && (getx p >= xmin)
     yok = (gety p <= ymax) && (gety p >= ymin)
     (xmin, xmax, ymin, ymax) = bounds (p1, p2)



bounds :: LineSeg -> (Float, Float, Float, Float)
bounds (p1, p2) = (xmin, xmax, ymin, ymax)
     where
     xmin = min (getx p1) (getx p2)
     xmax = max (getx p1) (getx p2)
     ymin = min (gety p1) (gety p2)
     ymax = max (gety p1) (gety p2)



chunk :: Int -> [a] -> [[a]]
chunk n xs
     | length xs >= n = [chnk] ++ (chunk n rest)
     | length xs == 0 = []
     | otherwise = [xs]
     where (chnk, rest) = splitAt n xs


fixdp n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
fix = fixdp 3

getx = fst
gety = snd
inf = read "Infinity" :: Float

triangle0 = triangle (0,0.25) (-0.5,-0.25) (0.5,-0.25)
triangle1 = triangle (0,0.95) (-0.5,0) (0.5,0)
triangle2 = triangle (0,0.75) (-0.5,0.25) (0.5,0.25)
triangle3 = triangle (0,0.9) (-0.2,-0.6) (0.2,-0.6)
triangle4 = triangle (0.1,0.9) (-0.1,-0.6) (0.2,-0.6)

test = do
     let inputs = [triangle2, triangle3]
     let s = perimeterLines inputs
     print "inputs:"
     print inputs
     print ""
     let allSegs = concat inputs
     print "allSegs:"
     print allSegs
     let cutter = (flip cutShapeWithShape) allSegs
     let cutShapes = zip inputs (map cutter inputs)
     print "cutShapes:"
     print $  map fst cutShapes
     print $  map snd cutShapes
     print ""
     let lineRemover lines shape = excludeContained shape lines
     let removeLines lines = foldl lineRemover lines
     let linesRemoved = map (\(shape,lines) -> removeLines lines (inputs \\ [shape] )) cutShapes

     print "LinesRemoved:"
     print linesRemoved
     print "output:"
     print s
     print "done"
     return s
