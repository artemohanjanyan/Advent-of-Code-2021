import Data.Function (on)
import Data.List (sort, sortBy, minimumBy, group)
import Text.Parsec hiding (State)

type InputParser = Parsec String ()

height :: InputParser Int
height = read . (:[]) <$> digit

inputParser :: InputParser [[Int]]
inputParser = sepBy (many height) newline

solve :: [[Int]] -> Int
solve heights = ans
  where
    getHeight (x, y) = heights !! x !! y
    points = [(x, y) | x <- [0..length heights - 1], y <- [0..length (head heights) - 1]]
    neightbourList = [(-1, 0), (0, -1), (1, 0), (0, 1)]
    inside (x, y) = x >= 0 && x <= (length heights - 1) && y >= 0 && y <= (length (head heights) - 1)
    neightbours (x, y) = filter inside $ map (\(dx, dy) -> (x + dx, y + dy)) neightbourList
    lower p1 p2 = getHeight p1 < getHeight p2
    go p = case filter (`lower` p) $ neightbours p of
      [] -> p
      ps -> go $ minimumBy (compare `on` getHeight) ps
    ans = product $ take 3 $ sortBy (flip compare) $ map length $ group $ sort $ map go $ filter ((/= 9) . getHeight) points

main :: IO ()
main = do
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> print $ solve input
    Left error -> print error
