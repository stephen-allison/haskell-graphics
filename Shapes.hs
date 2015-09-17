module Shapes where
import Data.List

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

cutLine :: LineSeg -> LineSeg -> [LineSeg]
cutLine tocut cutter = maybeInsertPoint tocut $ crossing tocut cutter

cutLine2 :: LineSeg -> LineSeg -> [LineSeg]
cutLine2 a b = (cutLine a b) ++ (cutLine b a)

merge :: Shape -> Shape -> Shape
merge s1 s2 =  cuts
     where
     cuts1 = mconcat $ map longest $ chunk 3 $ cutLine <$> s1 <*> s2
     cuts2 = mconcat $ map longest $ chunk 3 $ cutLine <$> s2 <*> s1
     filtered1 = filter (not . linesegInShape s2) cuts1
     filtered2 = filter (not . linesegInShape s1) cuts2
     cuts = nub (filtered1 ++ filtered2)

linesegInShape s (p1, p2) = inShape p1 s || inShape p2 s

longer :: [a] -> [a] -> [a]
longer a b = if (length a > length b) then a else b

longest :: [[a]] -> [a]
longest = foldr longer []

join :: LineSeg -> LineSeg -> [Point]
join (p1,p2) (p3,p4) = nub . sortOn distance $ [p1,p2,p3,p4]
     where distance = dist p1

dist (x1,y1) (x2,y2) = sqrt ((x2-x1)**2 + (y2-y1)**2)

pointsInShape s = nub  foldr (\(p1,p2) acc -> p1:p2:acc) [] s

chunk :: Int -> [a] -> [[a]]
chunk n xs
     | length xs >= n = [chnk] ++ (chunk n rest)
     | length xs == 0 = []
     | otherwise = [xs]
     where (chnk, rest) = splitAt n xs

crossing :: LineSeg -> LineSeg -> Maybe Point
crossing seg1 seg2
     | slope seg1 == slope seg2 = Nothing
     | slope seg1 == inf = bounded (getx p1, (gety p3) + (slope seg2) * (getx p3 - getx p1))
     | slope seg2 == inf = crossing seg2 seg1
     | otherwise = bounded (x, y)
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


inShape :: Point -> Shape -> Bool
inShape p s = val `mod` 2 == 1
     where
     val = sum (map maybeVal crossings)
     crossings = crossing <$> [toedge] <*> s
     toedge = (p, (100, gety p))

maybeVal Nothing = 0
maybeVal (Just _) = 1

slope :: LineSeg -> Float
slope (p1, p2)
     | dx == 0 = inf
     | otherwise = (gety p2 - gety p1) / dx
     where
     dx = (getx p2 - getx p1)

maybeInsertPoint :: LineSeg -> Maybe Point -> [LineSeg]
maybeInsertPoint seg Nothing = [seg]
maybeInsertPoint seg (Just p) = [s1, s2]
     where (s1, s2) = insertPoint seg p

insertPoint :: LineSeg -> Point -> (LineSeg, LineSeg)
insertPoint (p1, p2) newPt = ((p1,newPt), (newPt,p2))

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


getx = fst
gety = snd
inf = read "Infinity" :: Float

triangle1 = triangle (0,0.5) (0.5,0) (-0.5,0)
triangle2 = triangle (0,0.75) (0.5,0.25) (-0.5,0.25)

test = do
     let p1 = (0,0)
     let p2 = (3,3)
     let p3 = (0,3)
     let p4 = (3,0)
     putStrLn $ show $ crossing (p1,p2) (p3,p4)
     putStrLn $ show $ crossing (p1,p3) (p2,p4)

