{-# LANGUAGE RecordWildCards #-}

module SecondModel where

import           AutoDiff
import           GivenData

data Parameters a =
    Parameters { lambda :: a
               , c3     :: a
               , k      :: a
               , w      :: a
               , c4     :: a
               -- , p1     :: Point a
               -- , p2     :: Point a
               -- , p3     :: Point a
               -- , p4     :: Point a
               } deriving Show

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
          { lambda = newton zeta (Left 1000) initLambda
          , c3 = c3F (lambda parameters)
          , k = kF (c3 parameters) (lambda parameters)
          , w = newton ( phi (constDual $ c3 parameters)
                             (constDual $ k parameters)
                             (constDual $ lambda parameters) )
                       (Left 1000)
                       -- ((<= 10**(-13)) . abs . phi (k parameters) (lambda parameters))
                       312337.00320604927
          , c4 = c4F (c3 parameters) (k parameters) (lambda parameters) (w parameters)
          }


predict :: (Ord a, Floating a) => Parameters a -> a -> a
predict Parameters{..} t
  | 0 <= t && t <= 9  = let h = lambda / m in lambda * exp (h*c3) / (exp (h*t) - k * exp (h*c3))
  | 9 <  t && t <= 26 = let r = sqrt $ 4*k*w - lambda^2 in (r * tan (r * (c4 - t) / (2 * m)) - lambda) / (2 * k)
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
