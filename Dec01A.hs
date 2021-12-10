{-# LANGUAGE TypeApplications #-}

module Main where

main :: IO ()
main = do
  input <- (map (read @Int) . lines) <$> getContents
  print $ sum $ map (\(a, b) -> if a < b then 1 else 0) $ zip input $ tail input
