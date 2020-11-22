-- Approximate Pi

import System.Random

approxPi :: [(Double, Double)] -> Double
approxPi rands  = 4.0 * (divide $ countTrue $ map inCircle rands)
  where
    inCircle (x, y) = (x * x + y * y) <= 1.0
    divide   (x, y) = (fromIntegral x) / (fromIntegral y)
    acc b (true, n) = (true + (fromEnum b), n + 1)
    countTrue bools = foldr acc (0, 0) bools

randomPoints :: Int -> IO [(Double, Double)]
randomPoints 0 = return []
randomPoints n = do
  x <- randomRIO (0.0, 1.0)
  y <- randomRIO (0.0, 1.0)
  pts <- randomPoints (n - 1)
  return ((x, y) : pts)

main = do
  setStdGen (mkStdGen 271828)
  pts <- (randomPoints 9999991)
  print $ approxPi pts
