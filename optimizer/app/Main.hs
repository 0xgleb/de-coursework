import           Data.List       (sort, sortOn)
import           Data.Proxy

import           GivenData
import           Solution.Model  as Model
import qualified Solution.Second as Sol

adjustLength :: (a -> String) -> (a -> String -> b) -> [a] -> [b]
adjustLength g s l =
  (\x -> let str = g x in s x $ str ++ replicate (longest - length str) ' ') <$> l
  where longest = maximum $ length . g <$> l

showPredictions :: (ModelParams f, Show (f Double)) => f Double -> String
showPredictions params =
  foldMap (\(x, y, z, w) -> "| " ++ x ++ " | " ++ y ++ " | " ++ z ++ " | " ++ w ++ " |\n")
     $  adjustLength (\(_, _, _, w) -> w) (\(x, y, z, _) w -> (x, y, z, w))
     $  adjustLength (\(_, _, z, _) -> z) (\(x, y, _, w) z -> (x, y, z, w))
     $  adjustLength (\(_, y, _, _) -> y) (\(x, _, z, w) y -> (x, y, z, w))
     $  adjustLength (\(x, _, _, _) -> x) (\(_, y, z, w) x -> (x, y, z, w))
     $  (("Time", "Predicted Speed", "Rounded predicted speed", "Observed Speed"):)
     $  (\(Point t v) -> let v' = predict params t in (show t, show v', rounded v', rounded v))
    <$> points
  where rounded = show . round

maxErr :: (a -> Double -> Double) -> a -> Double
maxErr predict params =
  maximum $ abs . (\(Point t v) -> predict params t - v) <$> points
  where n = fromIntegral (length points)

avrErr :: (a -> Double -> Double) -> a -> Double
avrErr predict params =
  (/ n) $ sum $ abs . (\(Point t v) -> predict params t - v) <$> points
  where n = fromIntegral (length points)

rms :: (a -> Double -> Double) -> a -> Double
rms predict params =
  (/ n) $ sqrt $ sum $ (^2) . (\(Point t v) -> predict params t - v) <$> points
  where n = fromIntegral (length points)

showErrors :: (ModelParams f, Show (f Double)) => f Double -> String
showErrors params =
  foldMap (\(x, y, z) -> "| " ++ x ++ " | " ++ y ++ " | " ++ z ++ " |\n")
  $  adjustLength (\(_, _, z) -> z) (\(x, y, _) z -> (x, y, z))
  $  adjustLength (\(_, y, _) -> y) (\(x, _, z) y -> (x, y, z))
  $  adjustLength (\(x, _, _) -> x) (\(_, y, z) x -> (x, y, z))
  [ ("Maximum Error", "Average Error", "Root mean square")
  , (show $ maxErr predict params, show $ avrErr predict params, show $ rms predict params)
  ]

showResults :: (ModelParams f, Show (f Double)) => f Double -> String
showResults params = show params ++ "\n"
                  ++ showErrors params ++ "\n"
                  ++ showPredictions params

results :: [Sol.Parameters Double]
results =  sortOn (rms predict) $ findParams
      <$> combinations (Proxy :: Proxy Sol.Parameters)

main :: IO ()
main = putStr $ showResults $ head results
