module Main where

import           Data.List  (sortOn)
import qualified SecondModel as Model
import           GivenData

quadraticError :: (a -> Double -> Double) -> a -> Double
quadraticError predict params = (/ fromIntegral (length points)) $ sum
                              $ ((^2) . \(Point t v) -> predict params t - v) <$> points

showResults :: Model.Parameters Double -> String
showResults params = show params
                  ++ "\n"
                  ++ show (quadraticError Model.predict params)
                  ++ "\n"
                  ++ foldMap ( (++ "\n")
                             . show
                             . (\(t, v', v)->(t, v', round v', v))
                             . (\(Point t v) -> (t, Model.predict params t, v))
                             ) points

params :: [Model.Parameters Double]
params = sortOn (quadraticError Model.predict) $ Model.findParams <$> Model.combinations

main :: IO ()
main = putStrLn $ showResults (head params)
