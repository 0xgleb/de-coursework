{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Network.Types where

import AutoDiff

import Data.Vector.Sized
import GHC.TypeLits

import Control.Lens

type Weights     n = Vector n Number
type DualWeights n = Vector n (Dual Number)
type Activations n = Vector n Number

type NZ x x' = x ~ (x' + 1)

type Number = Float

type Bias          = Number
type Input         = Number
type Output        = Number

type Loss          = Number
type Iterations    = Integer
type LearningRate  = Number
data StopCriteria  = StopCriteria { _maxLoss      :: Loss
                                  , _maxIteration :: Iterations
                                  }

makeLenses ''StopCriteria


data LossFunction m where
    LossFunction :: { unLossF :: Vector m (Dual Output) -> Vector m (Dual Output) -> Dual Number } -> LossFunction m


data TotalLossFunction m o where
    TotalLossFunction :: { unTotLossF :: Vector m (Vector o Output) -> Vector m (Vector o Output) -> Output } -> TotalLossFunction m o


data Example i o where
    Example :: { _input  :: Vector i Number
               , _output :: Vector o Output
               } -> Example i o

makeLenses ''Example
