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

kF :: Floating a => Point a -> a -> a -> a
kF (Point t1 v1) c3 lambda = exp (lambda / m * (t1 - c3)) - lambda / v1

zeta :: Floating a => Point a -> Point a -> a -> a
zeta p1@(Point t1 v1) p2@(Point t2 v2) lambda =
  55 * (exp (h * 9) - k * exp (h * c3)) / exp (h * c3) - lambda
  where c3 = c3F p1 p2 lambda
        k  = kF  p1 c3 lambda
        h  = lambda / m

c4F :: Floating a => Point a -> a -> a -> a -> a
c4F (Point t4 v4) k lambda w =
  t4 + 2 * m * atan ((2*k*v4 + lambda) / denom) / denom
  where denom = sqrt (4*k*w - lambda^2)

phi :: Floating a => Point a -> a -> a -> a -> a
phi p4@(Point t4 v4) k lambda w =
  root * (c4 - t4) / (2*m) - atan ((2*k*v4 + lambda) / root)
  where c4   = c4F p4 k lambda w
        root = sqrt $ 4*k*w - lambda^2

findParams :: (Floating a, Ord a) => (Point a, Point a, Point a) -> Parameters a
findParams (p1, p2, p4) = parameters
  where parameters =
          Parameters { lambda = newton (zeta (toDualPoint p1) (toDualPoint p2)) ((<= 0.0001) . abs . zeta p1 p2) (-62.5)
                     , c3     = c3F p1 p2 (lambda parameters)
                     , k      = kF p1 (c3 parameters) (lambda parameters)
                     , w      = newton (phi (toDualPoint p4) (constDual $ k parameters) (constDual $ lambda parameters)) ((<= 0.0001) . abs . phi p4 (k parameters) (lambda parameters)) 301257.94278185006
                     , c4     = c4F p4 (k parameters) (lambda parameters) (w parameters)
                     }


predictSpeed :: Parameters Double -> Double -> Double
predictSpeed Parameters{..} t
  | 0 <= t && t <= 9  = let h = lambda / m in lambda * exp (h*c3) / (exp (h*t) - k * exp (h*c3))
  | 9 <  t && t <= 26 = let r = sqrt $ 4*k*w - lambda^2 in (r * tan (r * (c4 - t) / (2 * m)) - lambda) / (2 * k)
  | otherwise = error "Invalid time!"
