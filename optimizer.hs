{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.List (sortOn)

vs :: [Integer]
vs = [96,89,82,77,72,68,64,61,58,55,50,46,41,38,34,31,27,24,21,18,16,13,10,8,5,3,0]

data GivenPoint =
    GivenPoint { gpTime  :: Integer
               , gpSpeed :: Integer
               } deriving (Show, Eq)

points :: [GivenPoint]
points = zipWith GivenPoint [0..26] vs

newtype ResFactor = ResFactor { unResFactor :: Double } deriving (Show, Eq, Num, Ord)
newtype IntConst  = IntConst  { unIntConst  :: Double } deriving (Show, Eq, Num, Ord)

mass :: Integer
mass = 12*10^4

predictSpeed :: ResFactor -> IntConst -> Integer -> Double
predictSpeed rf ic time
  | 0 <= time && time < 9 = fromInteger mass / (unResFactor rf * fromInteger time - unIntConst ic)
  | otherwise = 0

loss :: ResFactor -> IntConst -> Double
loss rf ic = sum $ (^2) <$> zipWith (-) (fromInteger . gpSpeed <$> points) (predictSpeed rf ic . gpTime <$> points)

getResFactor :: GivenPoint -> GivenPoint -> ResFactor
getResFactor (GivenPoint t1 v1) (GivenPoint t2 v2) =
    ResFactor $ fromInteger (mass * (v2 - v1)) / fromInteger (v1 * v2 * (t1 - t2))

getIntConst :: GivenPoint -> GivenPoint -> IntConst
getIntConst (GivenPoint t1 v1) (GivenPoint t2 v2) =
    IntConst $ fromInteger (mass * (t2*v2 - t1*v1)) / fromInteger (v1 * v2 * (t1 - t2))

main :: IO ()
main =  print $  head $ sortOn (\(_, _, l) -> l)
     $  (\(rf, ic) -> (rf, ic, loss rf ic)) . (\(p1, p2) -> (getResFactor p1 p2, getIntConst p1 p2))
    <$> filter (uncurry (/=)) [ (p1, p2) | p1 <- somePoints, p2 <- somePoints ]
     where somePoints = filter (\p -> gpTime p < 9) points
