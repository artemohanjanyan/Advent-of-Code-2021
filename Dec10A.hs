import Data.Functor (void)
import Data.List (sort)
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

solve :: String -> Int
solve str = case parse chunkParser "" str of
  Right _ -> 0
  Left error -> case errorMessages error of
    SysUnExpect "\")\"" : _ -> 3
    SysUnExpect "\"]\"" : _ -> 57
    SysUnExpect "\"}\"" : _ -> 1197
    SysUnExpect "\">\"" : _ -> 25137
    _ -> 0

main :: IO ()
main = do
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> print $ sum $ map solve input
    Left error -> print error
