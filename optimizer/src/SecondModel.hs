{-# LANGUAGE RecordWildCards #-}

module SecondModel where

import           AutoDiff
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           GivenData

data Parameters a =
    Parameters { pLambda :: a
               , pC3     :: a
               , pK      :: a
               , pW      :: a
               , pC4     :: a
               } deriving (Show, Eq)

paramToVector :: Parameters a -> Vector a
paramToVector Parameters{..} = V.singleton pLambda `V.snoc` pC3 `V.snoc` pK `V.snoc` pW `V.snoc` pC4

vectorToParam :: Vector a -> Parameters a
vectorToParam vector =
  Parameters { pLambda = vector ! 0
             , pC3     = vector ! 1
             , pK      = vector ! 2
             , pW      = vector ! 3
             , pC4     = vector ! 4
             }

residuals :: (Floating a, Enum a, Ord a) => Vector a -> Vector a
residuals b = (\(Point t v) -> predict (vectorToParam b) t - v) <$>  vectorPoints

c3F :: (Floating a, Enum a, Ord a) => a -> a
c3F lambda = (\l -> m * log (product l) / (lambda * fromIntegral (length l))) $ uncurry c3F'
          <$> filter (\(p1, p2) -> pTime p1 > pTime p2)
                     [ (p1, p2) | p1 <- take 9 points, p2 <- take 9 points]
  where c3F' (Point t1 v1) (Point t2 v2) =
          (v1 * v2 * (exp (lambda / m * t1) - exp (lambda / m * t2))) / (lambda * (v2 - v1))

-- c3F :: Floating a => Point a -> Point a -> a -> a
-- c3F (Point t1 v1) (Point t2 v2) lambda =
--   m / lambda * log ((v1 * v2 * (exp (lambda / m * t1) - exp (lambda / m * t2))) / (lambda * (v2 - v1)))

kF :: (Floating a, Eq a, Enum a) => a -> a -> a
kF c3 lambda = (/10) $ sum $ (\Point{..} -> exp (lambda / m * (pTime - c3)) - lambda / pSpeed)
            <$> take 10 points

quadraticError :: (Num a, Enum a) => [Point a] -> (Point a -> a -> a) -> a -> a
quadraticError ps f u = sum $ (^2) . flip f u <$> ps

zeta :: (Floating a, Ord a, Enum a) => a -> a
zeta = d $ quadraticError (take 10 points) j
  where j (Point t v) lambda = lambda * exp (h*c3) / (exp (h*t) - k * exp (h*c3)) - v
          where c3 = c3F lambda
                k = kF c3 lambda
                h = lambda / m

-- zeta :: (Floating a, Ord a, Enum a) => Point a -> Point a -> a -> a
-- zeta p1@(Point t1 v1) p2@(Point t2 v2) = d $ quadraticError (take 10 points) j
--   where j (Point t v) lambda = lambda * exp (h*c3) / (exp (h*t) - k * exp (h*c3)) - v
--           where c3 = c3F lambda
--                 k = kF c3 lambda
--                 h = lambda / m

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


-- phi :: Floating a => Point a -> a -> a -> a -> a
-- phi (Point t4 v4) k lambda w = quadraticError (drop 10 points) j
--   root * (c4 - t4) / (2*m) - atan ((2*k*v4 + lambda) / root)
--   where c4 = c4F k lambda w
--         root = sqrt $ 4*k*w - lambda^2

-- phi :: (Floating a, Enum a, Eq a) => a -> a -> a -> a
-- phi k lambda = toNormalF $ quadraticError (drop 10 points) j
--   -- where j (Point t v) w = tan (r * (c4 - t) / (2 * m)) - (2 * v * constDual k + constDual lambda) / r
--   where j (Point t v) w = (r * tan (r * (c4 - t) / (2 * m)) - constDual lambda) / (2 * constDual k) - v
--           where c4 = c4F (constDual k) (constDual lambda) w
--                 r = sqrt $ 4 * constDual k * w - constDual lambda ^ 2

findParams :: (Floating a, Ord a, Enum a) => () -> Parameters a
findParams () = parameters
  where initLambda = 542.177309345586
        parameters = Parameters
          { pLambda = newton zeta (Left 1000) initLambda
          , pC3 = c3F (pLambda parameters)
          , pK = kF (pC3 parameters) (pLambda parameters)
          , pW = newton ( phi (constDual $ pC3 parameters)
                              (constDual $ pK parameters)
                              (constDual $ pLambda parameters) )
                        (Left 1000)
                        -- ((<= 10**(-13)) . abs . phi (k parameters) (lambda parameters))
                        312337.00320604927
          , pC4 = c4F (pC3 parameters) (pK parameters) (pLambda parameters) (pW parameters)
          }

initParams :: Num a => Parameters a
initParams =
  Parameters { pLambda = 700
             , pC3 = -21030
             , pK = 103
             , pW = 300000
             , pC4 = 25
             }

predict :: (Ord a, Floating a) => Parameters a -> a -> a
predict Parameters{..} t
  | 0 <= t && t <= 9  = let h = pLambda / m in pLambda * exp (h*pC3) / (exp (h*t) - pK * exp (h*pC3))
  | 9 <  t && t <= 26 = let r = sqrt $ 4*pK*pW - pLambda^2 in (r * tan (r * (pC4 - t) / (2 * m)) - pLambda) / (2 * pK)
  | otherwise = error "Invalid time!"


combinations :: [()]
combinations = [()]
-- combinations = zipWith (\p3 (p1, p2) -> (p1, p2, p3)) (drop 10 points)
--              $ filter (uncurry (/=))
--              $ (\l -> [ (p1, p2) | p1 <- l, p2 <- l ])
--              $ take 10 points
-- combinations = zipWith (\p4 (p1, p2, p3) -> (p1, p2, p3, p4)) (drop 10 points)
--              $ filter (\(p1, p2, _) -> p1 /= p2)
--              $ (\l -> [ (p1, p2, p3) | p1 <- l, p2 <- l, p3 <- l ])
--              $ take 10 points
