import Data.Functor (void)
import Data.List (sort)
import Text.Parsec hiding (State)

type InputParser = Parsec String ()

height :: InputParser Int
height = read . (:[]) <$> digit

inputParser :: InputParser [[Int]]
inputParser = sepBy (many height) newline

solve :: [[Int]] -> Int
solve heights = ans
  where
    ans = sum $ map (\(x, y) -> heights !! x !! y + 1) $ filter isLowPoint points
    points = [(x, y) | x <- [0..length heights - 1], y <- [0..length (head heights) - 1]]
    neightbourList = [(-1, 0), (0, -1), (1, 0), (0, 1)]
    inside (x, y) = x >= 0 && x <= (length heights - 1) && y >= 0 && y <= (length (head heights) - 1)
    neightbours (x, y) = filter inside $ map (\(dx, dy) -> (x + dx, y + dy)) neightbourList
    lower (x1, y1) (x2, y2) = heights !! x1 !! y1 < heights !! x2 !! y2
    isLowPoint p = all (lower p) $ neightbours p

main :: IO ()
main = do
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> print $ solve input
    Left error -> print error
