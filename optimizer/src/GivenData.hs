{-# LANGUAGE RecordWildCards #-}

module GivenData where

import           AutoDiff

vs :: Num a => [a]
vs = [96,89,82,77,72,68,64,61,58,55,50,46,41,38,34,31,27,24,21,18,16,13,10,8,5,3,0]

data Point a =
    Point { pTime  :: a
          , pSpeed :: a
          } deriving (Show, Eq)

points :: (Num a, Enum a) => [Point a]
points = zipWith Point [0..26] vs

m :: Num a => a
m = 120000

toDualPoint :: Num a => Point a -> Point (Dual a)
toDualPoint Point{..} = Point (constDual pTime) (constDual pSpeed)
