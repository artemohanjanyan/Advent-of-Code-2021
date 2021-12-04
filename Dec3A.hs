import Data.List (transpose)

main :: IO ()
main = do
  report <- lines <$> getContents
  let columns = transpose report
      count x = length . filter (== x)
      gammaBit column = if count '1' column > count '0' column then '1' else '0'
      gammaBits = map gammaBit columns
      epsilonBits = map (\x -> if x == '1' then '0' else '1') gammaBits

      toDecStep :: Int -> Char -> Int
      toDecStep n '0' = n * 2
      toDecStep n '1' = n * 2 + 1
      toDecStep _ b = error $ b : " is not a bit"

      gamma = foldl toDecStep 0 gammaBits
      epsilon = foldl toDecStep 0 epsilonBits
  print $ gamma * epsilon
