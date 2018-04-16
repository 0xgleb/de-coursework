{-# LANGUAGE RecordWildCards #-}

module SecondModel where

import           AutoDiff
import           Data.List (sortOn)
import           GivenData

data Parameters a =
    Parameters { lambda :: a
               , c3     :: a
               , k      :: a
               , w      :: a
               , c4     :: a
               } deriving Show

c3F :: Floating a => Point a -> Point a -> a -> a
c3F (Point t1 v1) (Point t2 v2) lambda =
  m / lambda * log ((v1 * v2 * (exp (lambda / m * t1) - exp (lambda / m * t2))) / (lambda * (v2 - v1)))

kF :: Floating a => a -> a -> a
kF c3 lambda = exp (lambda / m * (9 - c3)) - lambda / 55

zeta :: Floating a => Point a -> Point a -> Point a -> a -> a
zeta p1@(Point t1 v1) p2@(Point t2 v2) p3@(Point t3 v3) lambda =
  v3 * (exp (h * t3) - k * exp (h * c3)) / exp (h * c3) - lambda
  where c3 = c3F p1 p2 lambda
        k = kF c3 lambda
        h = lambda / m

c4F :: Floating a => a -> a -> a -> a
c4F k lambda w =
  9 + 2 * m * atan ((110*k + lambda) / denom) / denom
  where denom = sqrt (4*k*w - lambda^2)

phi :: Floating a => Point a -> a -> a -> a -> a
phi (Point t4 v4) k lambda w =
  root * (c4 - t4) / (2*m) - atan ((2*k*v4 + lambda) / root)
  where c4 = c4F k lambda w
        root = sqrt $ 4*k*w - lambda^2

findParams :: (Floating a, Ord a) => (Point a, Point a, Point a, Point a) -> Parameters a
findParams (p1, p2, p3, p4) = parameters
  where parameters = Parameters
          { lambda = newton (zeta (toDualPoint p1) (toDualPoint p2) (toDualPoint p3)) ((<= 10**(-12)) . abs . zeta p1 p2 p3) (-90)
          , c3 = c3F p1 p2 (lambda parameters)
          , k = kF (c3 parameters) (lambda parameters)
          , w = newton (phi (toDualPoint p4) (constDual $ k parameters) (constDual $ lambda parameters))
                       ((<= 0.0001) . abs . phi p4 (k parameters) (lambda parameters))
                       300000
          , c4 = c4F (k parameters) (lambda parameters) (w parameters)
          }


predictSpeed :: Parameters Double -> Double -> Double
predictSpeed Parameters{..} t
  | 0 <= t && t <= 9  = let h = lambda / m in lambda * exp (h*c3) / (exp (h*t) - k * exp (h*c3))
  | 9 <  t && t <= 26 = let r = sqrt $ 4*k*w - lambda^2 in (r * tan (r * (c4 - t) / (2 * m)) - lambda) / (2 * k)
  | otherwise = error "Invalid time!"


combinations :: (Enum a, Num a, Eq a) => [(Point a, Point a, Point a, Point a)]
combinations = zipWith (\p4 (p1, p2, p3) -> (p1, p2, p3, p4)) (drop 10 points)
             $ filter (\(p1, p2, _) -> p1 /= p2)
             $ (\l -> [ (p1, p2, p3) | p1 <- l, p2 <- l, p3 <- l ])
             $ take 10 points
