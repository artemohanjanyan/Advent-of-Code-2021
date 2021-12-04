import Data.Functor (($>))
import Text.Parsec hiding (State)

data Command = Forward Int | Down Int | Up Int
  deriving (Show)

type CParser = Parsec String ()

commandParser :: CParser Command
commandParser =
  ( (string "forward" $> Forward) <|>
    (string "down" $> Down) <|>
    (string "up" $> Up)
  ) <*
  spaces <*>
  (read <$> many1 digit)

inputParser :: CParser [Command]
inputParser = sepBy commandParser newline

data State = State
  { sDepth :: Int
  , sPosition :: Int
  }

runCommand :: State -> Command -> State
runCommand (State depth position) command = case command of
  Forward n -> State depth (position + n)
  Down n -> State (depth + n) position
  Up n -> State (depth - n) position

main :: IO ()
main = do
  parsedCommands <- parse inputParser "" <$> getContents
  case parsedCommands of
    Right commands -> do
      let finalState = foldl runCommand (State 0 0) commands
      print (sDepth finalState * sPosition finalState)
    Left error -> print error
