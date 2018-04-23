{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Solution.First (Parameters, SolutionGen) where

import           Data.Proxy

import           AutoDiff
import           GivenData
import qualified Solution.Model as M

data Parameters a =
    Parameters { pC1  :: a
               , pK   :: a
               , pC2  :: a
               , pB   :: a
               , pGen :: SolutionGen a
               }

data SolutionGen a =
    SolutionGen { sgPoint1 :: Point a
                , sgPoint2 :: Point a
                , sgPoint3 :: Point a
                }

instance Show a => Show (SolutionGen a) where
    show SolutionGen{..} =
         ", p1 = (" ++ show (pTime sgPoint1) ++ ", " ++ show (pSpeed sgPoint1) ++ ")"
      ++ ", p2 = (" ++ show (pTime sgPoint2) ++ ", " ++ show (pSpeed sgPoint2) ++ ")"
      ++ ", p3 = (" ++ show (pTime sgPoint3) ++ ", " ++ show (pSpeed sgPoint3) ++ ")"


instance Show a => Show (Parameters a) where
    show Parameters{..} =   "c1 = " ++ show pC1
                       ++  ", k = " ++ show pK
                       ++ ", c2 = " ++ show pC2
                       ++  ", b = " ++ show pB
                       ++ show pGen
                       ++ "\n"


c1F :: Fractional a => Point a -> Point a -> a
c1F (Point t1 v1) (Point t2 v2) = m * (v1 * t1 - v2 * t2) / (v1 * v2 * (t2 - t1))

kF :: Fractional a => a -> a
kF c1 = (c1 + m / 55) / 9

c2F :: Floating a => a -> a -> a
c2F k b = m * atan (55 * k * sqrt (b / k) / b) + 9 * sqrt (b * k)

lambda :: Floating a => Point a -> a -> a -> a
lambda (Point t3 v3) k b = (c2F k b - sqrt (k * b) * t3) / m - atan (v3 * sqrt (k / b))

findParams :: (Floating a, Ord a) => SolutionGen a -> Parameters a
findParams gen@SolutionGen{..} = parameters
  where parameters =
          Parameters { pC1 = c1F sgPoint1 sgPoint2
                     , pK  = kF $ pC1 parameters
                     , pC2 = c2F (pK parameters) (pB parameters)
                     , pB  = newton (lambda (toDualPoint sgPoint3) (constDual $ pK parameters))
                                    (Right $ (<= 0.0001) . abs . lambda sgPoint3 (pK parameters))
                                    300000
                     , pGen = gen
                     }

predict :: Parameters Double -> Double -> Double
predict Parameters{..} t
  | 0 <= t && t <= 9  = m / (pK * t - pC1)
  | 9 <  t && t <= 26 = sqrt (pB / pK) * tan ((pC2 - sqrt (pK * pB) * t) / m)
  | otherwise = error "Invalid time!"


combinations :: (Enum a, Num a, Eq a) => [SolutionGen a]
combinations = zipWith (flip $ uncurry SolutionGen) (drop 10 points)
             $ filter (uncurry (/=))
             $ (\l -> [ (p1, p2) | p1 <- l, p2 <- l ])
             $ take 10 points


instance M.ModelParams Parameters where
    type SolutionGen Parameters = SolutionGen
    predict = predict
    findParams = findParams
    combinations Proxy = combinations
