import           Data.List  (sortOn)
import qualified SecondModel as Model
import           GivenData

quadraticError :: (a -> Double -> Double) -> a -> Double
quadraticError predict params = (/ fromIntegral (length points)) $ sum $ (^2) <$> (\(Point t v) -> predict params t - v) <$> points

main :: IO ()
main = (\params -> print params >> print (quadraticError Model.predictSpeed params) >> foldMap (print . (\t -> (\v -> (t, round v, v)) $ Model.predictSpeed params t) . pTime) points)
     $ head $ sortOn (quadraticError Model.predictSpeed) $ Model.findParams <$> Model.combinations
