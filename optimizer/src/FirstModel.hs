{-# LANGUAGE RecordWildCards #-}

module FirstModel where

import           AutoDiff
import           GivenData

data Parameters a =
    Parameters { c1 :: a
               , k  :: a
               , c2 :: a
               , b  :: a
               , p1 :: Point a
               , p2 :: Point a
               , p3 :: Point a
               }

instance Show a => Show (Parameters a) where
    show Parameters{..} =   "c1 = " ++ show c1
                       ++  ", k = " ++ show k
                       ++ ", c2 = " ++ show c2
                       ++  ", b = " ++ show b
                       ++ ", p1 = (" ++ show (pTime p1) ++ ", " ++ show (pSpeed p1) ++ ")"
                       ++ ", p2 = (" ++ show (pTime p2) ++ ", " ++ show (pSpeed p2) ++ ")"
                       ++ ", p3 = (" ++ show (pTime p3) ++ ", " ++ show (pSpeed p3) ++ ")"

c1F :: Fractional a => Point a -> Point a -> a
c1F (Point t1 v1) (Point t2 v2) = m * (v1 * t1 - v2 * t2) / (v1 * v2 * (t2 - t1))

kF :: Fractional a => a -> a
kF c1 = (c1 + m / 55) / 9

c2F :: Floating a => a -> a -> a
c2F k b = m * atan (55 * k * sqrt (b / k) / b) + 9 * sqrt (b * k)

lambda :: Floating a => Point a -> a -> a -> a
lambda (Point t3 v3) k b = (c2F k b - sqrt (k * b) * t3) / m - atan (v3 * sqrt (k / b))

findParams :: (Floating a, Ord a) => (Point a, Point a, Point a) -> Parameters a
findParams (p1, p2, p3) = parameters
  where parameters =
          Parameters { c1 = c1F p1 p2
                     , k  = kF $ c1 parameters
                     , c2 = c2F (k parameters) (b parameters)
                     , b = newton (lambda (toDualPoint p3) (constDual $ k parameters))
                                  (Right $ (<= 0.0001) . abs . lambda p3 (k parameters))
                                  300000
                     , p1 = p1
                     , p2 = p2
                     , p3 = p3
                     }

predict :: Parameters Double -> Double -> Double
predict Parameters{..} t
  | 0 <= t && t <= 9  = m / (k * t - c1)
  | 9 <  t && t <= 26 = sqrt (b / k) * tan ((c2 - sqrt (k * b) * t) / m)
  | otherwise = error "Invalid time!"


combinations :: (Enum a, Num a, Eq a) => [(Point a, Point a, Point a)]
combinations = zipWith (\p3 (p1, p2) -> (p1, p2, p3)) (drop 10 points)
             $ filter (uncurry (/=))
             $ (\l -> [ (p1, p2) | p1 <- l, p2 <- l ])
             $ take 10 points
