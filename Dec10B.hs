import Data.Functor (void)
import Data.List (sort, foldl')
import Data.Maybe (catMaybes)
import Text.Parsec hiding (State)
import Text.Parsec.Error

type InputParser = Parsec String ()

inputParser :: InputParser [String]
inputParser = sepBy (many (oneOf "()[]<>{}")) newline

chunkParser :: InputParser ()
chunkParser =
  string "(" *> chunkParser <* string ")" *> chunkParser <|>
  string "[" *> chunkParser <* string "]" *> chunkParser <|>
  string "{" *> chunkParser <* string "}" *> chunkParser <|>
  string "<" *> chunkParser <* string ">" *> chunkParser <|>
  pure ()

solveCorrupted :: String -> Int
solveCorrupted str = case parse chunkParser "" str of
  Right _ -> 0
  Left error -> case errorMessages error of
    SysUnExpect "\")\"" : _ -> 3
    SysUnExpect "\"]\"" : _ -> 57
    SysUnExpect "\"}\"" : _ -> 1197
    SysUnExpect "\">\"" : _ -> 25137
    _ -> 0

solveIncomplete :: String -> Maybe Int
solveIncomplete str = case solveCorrupted str of
  n | n > 0 -> Nothing
  n -> Just $ foldl' scoreStep 0 $ foldl' go [] str
  where
    go :: [Char] -> Char -> [Char]
    go st c
      | c == '(' || c == '[' || c == '{' || c == '<' = c : st
    go ('(':st) ')' = st
    go ('[':st) ']' = st
    go ('{':st) '}' = st
    go ('<':st) '>' = st
    go (c1:st) c2 = error "shouldn't be possible"
    go [] _ = error "shouldn't be possible"

    scoreStep :: Int -> Char -> Int
    scoreStep n c = n * 5 + cScore
      where
        cScore = case c of
          '(' -> 1
          '[' -> 2
          '{' -> 3
          '<' -> 4
          _ -> error "shouldn't be possible"

solve :: [String] -> Int
solve chunks = incompleteScores !! (len `div` 2)
  where
    incompleteScores = sort $ catMaybes $ map solveIncomplete chunks
    len = length incompleteScores

main :: IO ()
main = do
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> print $ solve input
    Left error -> print error
