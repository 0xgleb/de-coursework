{-# LANGUAGE DataKinds #-}

import           Control.Monad
import           Data.Maybe        (fromJust)
import           Data.Proxy
import           Data.Vector.Sized
import           GHC.TypeLits
import           Prelude           hiding (head, replicate, zipWith)

import           Network

identitySummation :: DualWeights 1 -> Dual Bias -> Activations 1 -> Dual Output
identitySummation _ _ inputs = constDual $ head inputs

constSummation :: DualWeights n -> Dual Bias -> Activations n -> Dual Output
constSummation _ _ _ = 0

m :: Num a => a
m  = 12*10^4

deSummation :: DualWeights 4 -> Dual Bias -> Activations 4 -> Dual Output
deSummation ws _ as
  | 0 <= t && t < 9   = m / (k * t - c1)
  | 9 <= t && t <= 26 = sqrt (b / k) * tan ((c2 - t * sqrt (k*b)) / m)
  | otherwise = error "Invalid time input!"
  where t  = constDual $ index' as (Proxy :: Proxy 0)
        k  = index' ws (Proxy :: Proxy 1)
        b  = index' ws (Proxy :: Proxy 2)
        c2 = index' ws (Proxy :: Proxy 3)
        c1 = findC1 k b c2

findC1 :: Floating a => a -> a -> a -> a
findC1 k b c2 = 9*k - m * sqrt (k / b) / tan ((c2 - 9 * sqrt (k*b)) / m)

makeNeuron :: KnownNat n => (DualWeights n -> Dual Bias -> Activations n -> Dual Output) -> Neuron n
makeNeuron sumF = Neuron sumF id (replicate 0) 0

layer1 :: Network 1 1 4
layer1 = (makeNeuron identitySummation `cons` replicate (makeNeuron constSummation))
     :~~ NilNetwork

net :: Network 1 2 1
net = (makeNeuron identitySummation `cons` replicate (makeNeuron constSummation))
  :~~ singleton (Neuron deSummation id (singleton 1 `snoc` 100 `snoc` 300997 `snoc` (1.215591*m)) 0)
  :~~ NilNetwork

examples :: Vector 27 (Example 1 1)
examples = zipWith (\t v -> singleton t `Example` singleton v) (unsafeFromList [0..26]) vs
  where unsafeFromList = fromJust . fromList
        vs = unsafeFromList [96,89,82,77,72,68,64,61,58,55,50,46,41,38,34,31,27,24,21,18,16,13,10,8,5,3,0]

main :: IO ()
main = do
    let martinsLoss = (unTotLossF $ getTotalLoss quadraticLoss) (_output <$> examples) $ runNetwork net . _input <$> examples
    print martinsLoss
    let smartNet = train quadraticLoss 1 examples (Left $ StopCriteria (0.95 * martinsLoss) 60000) net
    print $ (unTotLossF $ getTotalLoss quadraticLoss) (_output <$> examples) $ runNetwork smartNet . _input <$> examples
    print $ smartNet

-- main :: IO ()
-- main = do
--     smartNet <- train quadraticLoss 1 examples (Left $ StopCriteria 0.0000073 60000) <$> initNet testNet
--     print $ (unTotLossF $ getTotalLoss quadraticLoss) (map _output examples) $ map (runNetwork smartNet . _input) examples
--     putStrLn ""
--     foldr ((>>) . print . head . runNetwork smartNet . (^. input)) mempty examples
