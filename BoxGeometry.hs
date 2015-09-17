-- some really simple geometry calculations for lines constrained by a box
module BoxGeometry where

import Data.List
import Data.Maybe
import Data.Monoid

import Graphics.UI.GLUT

type N = GLfloat
type Point = (N, N)

data Box = Box {left::N, right::N, top::N, bottom::N} deriving Show
simpleBox :: Box
simpleBox = Box {left=(-1), right=1, top=1, bottom=(-1)}

x_ :: Point -> N
x_ (x, y) = x

y_ :: Point -> N
y_ (x, y) = y

reflect1 :: Point -> Point
reflect1 (x1, y1) = (y1, x1)



foldedPoints :: Box -> Point -> Point -> [Point]
foldedPoints box p1 p2 = p1 : (unfoldr (foldPoint box) (p1, p2))

foldPoint :: Box -> (Point, Point) -> Maybe (Point, (Point, Point))
foldPoint box (lastpt, nextpt)
              | lastpt == nextpt = Nothing
              | otherwise = Just (newnextpt, (newnextpt, flippedpt))
              where
                crosspt = crosspoint box lastpt nextpt
                (newnextpt, flippedpt) = case crosspt of Just p -> (p, flipPoint p nextpt)
                                                         Nothing -> (nextpt, nextpt)

flipPoint :: Point -> Point -> Point
flipPoint (cx, cy) (x, y)
    | abs cx == 1.0 = (cx - (x - cx), y)
    | abs cy == 1.0 = (x, cy - (y - cy))


-- find the first (and only) non-Nothing crossing point
crosspoint :: Box -> Point -> Point -> Maybe Point
crosspoint box p1 p2 = getFirst . mconcat . map First $ crossings box p1 p2

crossings :: Box -> Point -> Point -> [Maybe Point]
crossings box p1 p2 = map inBox' maybeCrossings
          where
          maybeCrossings = [xCrossing (left box) p1 p2,
                            xCrossing (right box) p1 p2,
                            yCrossing (top box) p1 p2,
                            yCrossing (bottom box) p1 p2]
          inBox' Nothing = Nothing
          inBox' (Just p)
                | inBox box p = Just p
                | otherwise = Nothing


lineInBox box p1 p2 = (inBox box p1) && (inBox box p2)




yCrossing :: N -> Point -> Point -> Maybe Point
yCrossing yVal p1 p2
        | (yValInRange yVal p1 p2) == True = Just (x1 + dx, yVal)
        | otherwise = Nothing
        where
          x1 = x_ p1
          y1 = y_ p1
          dy = yVal - y1
          dx = dy / (slope p1 p2)
          yValInRange yVal (xa,ya) (xb,yb) = between' (min ya yb) (max ya yb) yVal


xCrossing :: N -> Point -> Point -> Maybe Point
xCrossing xVal p1 p2 = reflect1 <$> yCrossing xVal (reflect1 p1) (reflect1 p2)

distance :: Point -> Point -> N
distance (x1,y1) (x2,y2) = sqrt ((x2 - x1)**2 + (y2 - y1)**2)

slope :: Point -> Point -> N
slope (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

inBox :: Box -> Point -> Bool
inBox box (x, y) = xInBounds && yInBounds
  where xInBounds = between (left box) (right box) x
        yInBounds = between (bottom box) (top box) y


between :: (Ord a, Num a) => a -> a -> a -> Bool
between min max val = (val >= min) && (val <= max)

between' :: (Ord a, Num a) => a -> a -> a -> Bool
between' min max val = (val > min) && (val < max)
