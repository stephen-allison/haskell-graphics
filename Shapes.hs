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

cutLine :: LineSeg -> LineSeg -> [LineSeg]
cutLine tocut cutter = maybeInsertPoint tocut $ crossing tocut cutter

cutLine2 :: LineSeg -> LineSeg -> [LineSeg]
cutLine2 a b = (cutLine a b) ++ (cutLine b a)

merge :: Shape -> Shape -> Shape
merge s1 s2 = (merge' s1 s2) ++ (merge' s2 s1)

merge' s1 s2 = filter (not . linesegInShape s2) $ concat segs
     where
     ccs = cc s1 s2
     pts = map (sort . nub . pointsInShape . concat) ccs
     segs = map (\ps -> zip ps (drop 1 ps)) pts

{-
     cuts1 = filt s2 $ cuts' s1 s2
     cuts2 = filt s1 $ cuts' s2 s1
     filt s = filter (not . linesegInShape s)
-}
{-
     let ccs = cc triangle2 triangle1
     let pts = sort $ nub $ pointsInShape $ concat $ ccs !! 1
     --putStrLn $ show $ sort $ nub $ pointsInShape $ concat $ ccs !! 0
     putStrLn $ show $ pts
     putStrLn $ show $ map ((flip inShape) triangle1) pts
     putStrLn $ show $ map (crossingsToEdge triangle1) pts
     --putStrLn $ show $ sort $ nub $ pointsInShape $ concat $ ccs !! 2
     let segs = zip pts (drop 1 pts)
     print "----"
     print segs
     --let filt = filter (not . linesegInShape triangle1)
     let r = map (linesegInShape triangle1) segs
     print $ r
-}


cuts' s1 s2 = nub . mconcat . mconcat $ chunk 3 $ cutLine <$> s1 <*> s2
cc s1 s2 = chunk (length s2) $ cutLine <$> s1 <*> s2

linesegInShape s (p1, p2) = inShape p1 s && inShape p2 s

longer :: [a] -> [a] -> [a]
longer a b = if (length a > length b) then a else b

longest :: [[a]] -> [a]
longest = foldr longer []

join :: LineSeg -> LineSeg -> [Point]
join (p1,p2) (p3,p4) = nub . sortOn distance $ [p1,p2,p3,p4]
     where distance = dist p1

dist (x1,y1) (x2,y2) = sqrt ((x2-x1)**2 + (y2-y1)**2)

pointsInShape s = foldr (\(p1,p2) acc -> p1:p2:acc) [] s

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
inShape p s = total /= 0
     where
     vals = map scorefn (crossingsToEdge s p)
     total = sum (map (\n -> n `mod` 2)  vals)
     verts = nub . pointsInShape $ s
     scorefn ps = length $ filt ps
          where
          filt = filter (\p -> not (p `elem` verts))



crossingsToEdge s p = map catMaybes $ edgeCrossings s p

edgeCrossings s p = chunk (length s) (crossing <$> toedge <*> s)
     where toedge = [(p, (100, gety p)),
                     (p, (getx p, 100)),
                     (p, (100 * getx p, 100 * gety p))]

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
triangle3 = triangle (-0.2,0.9) (0.2,0.9) (0,-0.9)


test = do
     let s1 = merge' triangle2 triangle1
     putStrLn $ show s1
     let s2 = merge' triangle1 triangle2
     putStrLn $ show s2



