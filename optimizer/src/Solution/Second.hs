{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Solution.Second (Parameters, SolutionGen) where

import           Data.Proxy

import           AutoDiff
import           GivenData
import qualified Solution.Model as M

data Parameters a =
    Parameters { pLambda :: a
               , pC3     :: a
               , pK      :: a
               , pW      :: a
               , pC4     :: a
               , pGen    :: SolutionGen a
               }

data SolutionGen a =
    SolutionGen { sgPoint1 :: Point a
                , sgPoint2 :: Point a
                }

instance Show a => Show (SolutionGen a) where
    show SolutionGen{..} =
         ", p1 = (" ++ show (pTime sgPoint1) ++ ", " ++ show (pSpeed sgPoint1) ++ ")"
      ++ ", p2 = (" ++ show (pTime sgPoint2) ++ ", " ++ show (pSpeed sgPoint2) ++ ")"


instance Show a => Show (Parameters a) where
    show Parameters{..} =    "λ = " ++ show pLambda
                       ++ ", c3 = " ++ show pC3
                       ++  ", K = " ++ show pK
                       ++  ", W = " ++ show pW
                       ++ ", c4 = " ++ show pC4
                       ++ show pGen
                       ++ "\n"

c3F :: Floating a => Point a -> Point a -> a -> a
c3F (Point t1 v1) (Point t2 v2) lambda =
  m / lambda * log ((v1*v2*(exp (lambda / m * t1) - exp (lambda / m * t2))) / (lambda * (v2 - v1)))

kF :: (Floating a, Ord a, Enum a) => a -> a -> a
kF c3 lambda = exp (lambda / m * (9 - c3)) - lambda / 55

quadraticError :: (Num a, Enum a) => [Point a] -> (Point a -> a -> a) -> a -> a
quadraticError ps f u = sum $ (^2) . flip f u <$> ps

zeta :: (Floating a, Ord a, Enum a) => Point a -> Point a -> a -> a
zeta p1 p2 = d $ quadraticError (take 10 points) j
  where j (Point t v) lambda = lambda * exp (h*c3) / (exp (h*t) - k * exp (h*c3)) - v
          where c3 = c3F (toDualPoint p1) (toDualPoint p2) lambda
                k = kF c3 lambda
                h = lambda / m

c4F :: Floating a => a -> a -> a -> a -> a
c4F c3 k lambda w =
  9 + 2 * m * atan ((2*v*k + lambda) / denom) / denom
  where denom = sqrt (4*k*w - lambda^2)
        h = lambda / m
        v = lambda * exp (h*c3) / (exp (h*9) - k * exp (h*c3))


phi :: (Floating a, Enum a, Eq a) => a -> a -> a -> a -> a
phi c3 k lambda = d $ quadraticError (drop 9 points) j
  where j (Point t v) w =
          root * (c4 - t) / (2*m) - atan ((2 * constDual k * v + constDual lambda) / root)
          where c4 = c4F (constDual c3) (constDual k) (constDual lambda) w
                root = sqrt $ 4 * constDual k * w - constDual lambda ^2

findParams :: (Floating a, Ord a, Enum a) => SolutionGen a -> Parameters a
findParams gen@SolutionGen{..} = parameters
  where parameters = Parameters
          { pLambda = newton (zeta (toDualPoint sgPoint1) (toDualPoint sgPoint2)) (Left 1000) 921.7
          , pC3 = c3F sgPoint1 sgPoint2 (pLambda parameters)
          , pK = kF (pC3 parameters) (pLambda parameters)
          , pW = newton ( phi (constDual $ pC3 parameters)
                              (constDual $ pK parameters)
                              (constDual $ pLambda parameters) )
                        (Left 1000)
                        290000
          , pC4 = c4F (pC3 parameters) (pK parameters) (pLambda parameters) (pW parameters)
          , pGen = gen
          }

predict :: (Ord a, Floating a) => Parameters a -> a -> a
predict Parameters{..} t
  | 0 <= t && t <= 9  =
    let h = pLambda / m in pLambda * exp (h*pC3) / (exp (h*t) - pK * exp (h*pC3))
  | 9 <  t && t <= 26 =
    let r = sqrt $ 4*pK*pW - pLambda^2 in (r * tan (r * (pC4 - t) / (2 * m)) - pLambda) / (2 * pK)
  | otherwise = error "Invalid time!"


combinations :: (Num a, Enum a, Eq a) => [SolutionGen a]
combinations = fmap (uncurry SolutionGen)
             $ filter (uncurry (/=))
             $ (\l -> [ (p1, p2) | p1 <- l, p2 <- l])
             $ take 10 points

instance M.ModelParams Parameters where
    type SolutionGen Parameters = SolutionGen
    predict = predict
    findParams = findParams
    combinations Proxy = combinations
