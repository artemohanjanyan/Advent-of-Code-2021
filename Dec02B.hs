{-# LANGUAGE RecordWildCards #-}

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
  , sAim :: Int
  }
  deriving (Show)

startPosition :: State
startPosition = State 0 0 0

runCommand :: State -> Command -> State
runCommand state@State{..} command = case command of
  Forward n -> state { sDepth = sDepth + sAim * n, sPosition = sPosition + n }
  Down n -> state { sAim = sAim + n }
  Up n -> state { sAim = sAim - n }

runCommands :: [Command] -> State
runCommands = foldl runCommand startPosition

main :: IO ()
main = do
  Right commands <- parse inputParser "" <$> getContents
  let finalState = runCommands commands
  print (sDepth finalState * sPosition finalState)
