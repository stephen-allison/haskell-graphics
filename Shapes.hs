module Shapes where
import Data.List
import Data.Maybe

type Point = (Float, Float)
type LineSeg = (Point, Point)
type Box = (Point, Point, Point, Point)
type Shape = [LineSeg]


lineSeg :: Float -> Float -> Float -> Float -> LineSeg
lineSeg x1 y1 x2 y2 = ((x1, y1), (x2, y2))



point :: Float -> Float -> Point
point = (,)



triangle :: Point -> Point -> Point -> Shape
triangle p1 p2 p3 = [(p1,p2), (p2,p3), (p3,p1)]



fuse :: [LineSeg] -> [LineSeg] -> [LineSeg]
fuse as bs = build rest [start]
     where start = head as
           rest = (tail as) ++ bs
           build [] built = reverse built
           build segs built = build updatedSegs updatedBuilt
               where updatedBuilt = nextSeg' : built
                     updatedSegs = segs \\ updatedBuilt
                     prevSeg = head built
                     nextSeg = find (\s -> fst s == snd prevSeg) segs
                     nextSeg' = case nextSeg
                                   of   Just s  -> s
                                        Nothing -> (snd.last $ built, fst start)

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
     | slope seg1 == inf = bounded (getx p1, (gety p3) + (slope seg2) * (getx p3 - getx p1))
     | slope seg2 == inf = crossing seg2 seg1
     | otherwise = bounded (fix x, fix y)
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
     bounded p = if (onLine p seg1) && (onLine p seg2) then Just p else Nothing



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
contains segs p = and $ map (isLeftOf p) segs



slope :: LineSeg -> Float
slope (p1, p2)
     | dx == 0 = inf
     | otherwise = (gety p2 - gety p1) / dx
     where
     dx = (getx p2 - getx p1)



onLine :: Point -> LineSeg -> Bool
onLine p (p1, p2) = xok && yok
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
triangle3 = triangle (0,0.9) (0.2,-0.6) (-0.2,-0.6)

test = do
     let cut12 = cutShapeWithShape triangle1 triangle2
     let cut21 = cutShapeWithShape triangle2 triangle1
     let ex12 = excludeContained triangle2 cut12
     let ex21 = excludeContained triangle1 cut21

     let fused = fuse ex12 ex21
     print fused
     print "done"
