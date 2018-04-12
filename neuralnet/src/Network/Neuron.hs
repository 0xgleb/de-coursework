{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Network.Neuron
( Neuron(..)
, module AutoDiff
, summation
, activation
, weights
, bias
, initNeuron
, scaleVector
, quadraticLoss
, runNeuron
) where

import Control.Lens hiding (cons)

import Prelude hiding ((++), head, tail, foldl, zipWith, map, sum, mapM)
import qualified Prelude as P ((++))
import Data.Vector.Sized
import GHC.TypeLits
import AutoDiff

import System.Random (randomRIO)

import Network.Types

data Neuron n where
    Neuron :: { _summation  :: DualWeights n -> Dual Bias -> Activations n -> Dual Output
              , _activation :: Dual Output -> Dual Output
              , _weights    :: Weights n
              , _bias       :: Bias
              } -> Neuron n

makeLenses ''Neuron


instance Show (Neuron n) where
    show neuron = "- weights: " +++ show (neuron ^. weights)
              +++ "\n- bias: " +++ show (neuron ^. bias)
      where (+++) = (P.++)

initNeuron :: Neuron n -> IO (Neuron n)
initNeuron (Neuron s a ws b) = (Neuron s a <$> mapM (\_ -> randomRIO (0, 1) :: IO Number) ws) <*> (randomRIO (0, 1) :: IO Number)

scaleVector :: Num a => a -> Vector n a -> Vector n a
scaleVector x = map (* x)

quadraticLoss :: LossFunction n
quadraticLoss = LossFunction $ \actual expected -> (/2) $ sum $ map (^2) $ zipWith (-) actual expected

runNeuron :: Neuron n -> Vector n Input -> Output
runNeuron neuron = val . (neuron ^. activation) . ((neuron ^. summation $ map constDual $ neuron ^. weights) $ constDual $ neuron ^. bias)
