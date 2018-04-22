{-# LANGUAGE TypeFamilies #-}

module Solution.Model where

import           Data.Proxy

class ModelParams f where
    type SolutionGen f :: * -> *
    predict :: f Double -> Double -> Double
    findParams :: (Floating a, Ord a, Enum a) => SolutionGen f a -> f a
    combinations :: (Num a, Enum a, Eq a) => Proxy f -> [SolutionGen f a]
