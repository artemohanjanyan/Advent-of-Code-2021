{-# LANGUAGE TypeApplications #-}

module Main where

main :: IO ()
main = do
  input <- (map (read @Int) . lines) <$> getContents
  let sums = zipWith3 (\a b c -> a + b + c) input (tail input) (tail $ tail input)
  print $ sum $ map (\(a, b) -> if a < b then 1 else 0) $ zip sums $ tail sums
