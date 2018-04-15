import           AutoDiff
import           Data.List (sortOn)

vs :: Num a => [a]
vs = [96,89,82,77,72,68,64,61,58,55,50,46,41,38,34,31,27,24,21,18,16,13,10,8,5,3,0]

data GivenPoint a =
    GivenPoint { gpTime  :: a
               , gpSpeed :: a
               } deriving (Show, Eq)

points :: (Num a, Enum a) => [GivenPoint a]
points = zipWith GivenPoint [0..26] vs

m :: Num a => a
m = 120000

c1F :: Fractional a => GivenPoint a -> GivenPoint a -> a
c1F (GivenPoint t1 v1) (GivenPoint t2 v2) = m * (v1 * t1 - v2 * t2) / (v1 * v2 * (t2 - t1))

kF :: Fractional a => a -> a
kF c1 = (c1 + m / 55) / 9

c2F :: Floating a => a -> a -> a
c2F k b = m * atan (55 * k * sqrt (b / k) / b) + 9 * sqrt (b * k)

lambda :: Floating a => a -> a -> a -> a -> a
lambda t3 v3 k b = (c2F k b - sqrt (k * b) * t3) / m - atan (v3 * sqrt (k / b))

findParams :: RealFloat a => (GivenPoint a, GivenPoint a, GivenPoint a) -> (a, a, a, a)
findParams (gp1, gp2, gp3) = (c1, k, c2, b)
  where c1 = c1F gp1 gp2
        k  = kF c1
        c2 = c2F k b
        b  = bF 250000
        bF x =
          let lF = lambda (constDual $ gpTime gp3) (constDual $ gpSpeed gp3) $ constDual k
              l  = toNormalF lF x
          in if isNaN l then bF (x+1) else if l >= 0.0001 then bF (x - l / d lF x) else x

predictSpeed :: (Floating a, Ord a) => (a, a, a, a) -> a -> a
predictSpeed (c1, k, c2, b) t
  | 0 <= t && t <= 9  = m / (k * t - c1)
  | 9 <  t && t <= 26 = sqrt (b / k) * tan ((c2 - sqrt (k * b) * t) / m)
  | otherwise = error "Invalid time!"


quadraticError :: (Floating a, Ord a, Enum a) => (a, a, a, a) -> a
quadraticError params = (/ fromIntegral (length points)) $ sum $ abs . (\(GivenPoint t v) -> predictSpeed params t - v) <$> points

main :: IO ()
main = (\params -> print params >> print (quadraticError params) >> foldMap (print . (\t -> (\v -> (t, round v, v)) $ predictSpeed params t) . gpTime) points) $ head $ sortOn (\params -> quadraticError params) $ findParams <$> combinations
  where combinations = zipWith (\gp3 (gp1, gp2) -> (gp1, gp2, gp3)) (drop 10  points) $ filter (uncurry (/=)) $ (\l -> [ (gp1, gp2) | gp1 <- l, gp2 <- l ]) $ take 10 points

-- identitySummation :: DualWeights 1 -> Dual Bias -> Activations 1 -> Dual Output
-- identitySummation _ _ inputs = constDual $ head inputs

-- constSummation :: DualWeights n -> Dual Bias -> Activations n -> Dual Output
-- constSummation _ _ _ = 0

-- m :: Num a => a
-- m  = 12*10^4

-- deSummation :: DualWeights 4 -> Dual Bias -> Activations 4 -> Dual Output
-- deSummation ws _ as
--   | 0 <= t && t < 9   = m / (k * t - c1)
--   | 9 <= t && t <= 26 = sqrt (b / k) * tan ((c2 - t * sqrt (k*b)) / m)
--   | otherwise = error "Invalid time input!"
--   where t  = constDual $ index' as (Proxy :: Proxy 0)
--         k  = index' ws (Proxy :: Proxy 1)
--         b  = index' ws (Proxy :: Proxy 2)
--         c2 = index' ws (Proxy :: Proxy 3)
--         c1 = findC1 k b c2

-- findC1 :: Floating a => a -> a -> a -> a
-- findC1 k b c2 = 9*k - m * sqrt (k / b) / tan ((c2 - 9 * sqrt (k*b)) / m)

-- makeNeuron :: KnownNat n => (DualWeights n -> Dual Bias -> Activations n -> Dual Output) -> Neuron n
-- makeNeuron sumF = Neuron sumF id (replicate 0) 0

-- layer1 :: Network 1 1 4
-- layer1 = (makeNeuron identitySummation `cons` replicate (makeNeuron constSummation))
--      :~~ NilNetwork

-- net :: Network 1 2 1
-- net = (makeNeuron identitySummation `cons` replicate (makeNeuron constSummation))
--   :~~ singleton (Neuron deSummation id (singleton 1 `snoc` 100 `snoc` 300997 `snoc` (1.215591*m)) 0)
--   :~~ NilNetwork

-- examples :: Vector 27 (Example 1 1)
-- examples = zipWith (\t v -> singleton t `Example` singleton v) (unsafeFromList [0..26]) vs
--   where unsafeFromList = fromJust . fromList
--         vs = unsafeFromList [96,89,82,77,72,68,64,61,58,55,50,46,41,38,34,31,27,24,21,18,16,13,10,8,5,3,0]

-- main :: IO ()
-- main = do
--     let martinsLoss = (unTotLossF $ getTotalLoss quadraticLoss) (_output <$> examples) $ runNetwork net . _input <$> examples
--     print martinsLoss
--     let smartNet = train quadraticLoss 1 examples (Left $ StopCriteria (0.95 * martinsLoss) 60000) net
--     print $ (unTotLossF $ getTotalLoss quadraticLoss) (_output <$> examples) $ runNetwork smartNet . _input <$> examples
--     print $ smartNet

-- main :: IO ()
-- main = do
--     smartNet <- train quadraticLoss 1 examples (Left $ StopCriteria 0.0000073 60000) <$> initNet testNet
--     print $ (unTotLossF $ getTotalLoss quadraticLoss) (map _output examples) $ map (runNetwork smartNet . _input) examples
--     putStrLn ""
--     foldr ((>>) . print . head . runNetwork smartNet . (^. input)) mempty examples
