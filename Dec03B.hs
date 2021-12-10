import Data.List (transpose)

count :: Char -> [String] -> Int
count x = length . filter ((== x) . head)

processStep
  :: (Int -> Int -> Bool)
  -> Char
  -> [(String, String)]
  -> [(String, String)]
processStep (?<) bias report =
  let
    zeroN = count '0' $ map fst report
    oneN = count '1' $ map fst report
    filterReport c = map (\(a, b) -> (tail a, b)) $ filter ((== c) . head . fst) report
  in
    if zeroN ?< oneN then filterReport '1'
    else if oneN ?< zeroN then filterReport '0'
    else filterReport bias

process :: (Int -> Int -> Bool) -> Char -> [String] -> String
process (?<) bias report =
  let
    goProcess report = 
      if length report == 1
        then snd $ head report
        else goProcess $ processStep (?<) bias report
  in goProcess $ map (\x -> (x, x)) report

toDecStep :: Int -> Char -> Int
toDecStep n '0' = n * 2
toDecStep n '1' = n * 2 + 1
toDecStep _ b = error $ b : " is not a bit"

toDec :: String -> Int
toDec = foldl toDecStep 0

main :: IO ()
main = do
  report <- lines <$> getContents
  print $ toDec (process (<) '1' report) * toDec (process (>) '0' report)
