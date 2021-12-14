{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Functor (($>))
import Data.List (foldl')
import System.Environment (getArgs)
import Text.Parsec hiding (State)

import qualified Data.Map as Map
import qualified Data.Set as Set

type InputParser = Parsec String ()

type Point = (Int, Int)

type Paper = Set.Set Point

data Fold = AlongX Int | AlongY Int

data Input = Input Paper [Fold]

pointsToPaper :: [Point] -> Paper
pointsToPaper = Set.fromList

intParser :: InputParser Int
intParser = read <$> many1 digit

pointParser :: InputParser Point
pointParser = (,) <$> intParser <* string "," <*> intParser

foldParser :: InputParser Fold
foldParser =
  string "fold along " *>
  (string "x" $> AlongX <|> string "y" $> AlongY) <*>
  (string "=" *> intParser)

inputParser :: InputParser Input
inputParser = Input <$>
  (pointsToPaper <$> many (pointParser <* newline)) <*
  newline <*> 
  sepBy foldParser newline

doFold :: Fold -> Paper -> Paper
doFold (AlongX foldX) = Set.map $ \(x, y) -> (foldX - abs (x - foldX), y)
doFold (AlongY foldY) = Set.map $ \(x, y) -> (x, foldY - abs (y - foldY))

solveA :: Input -> Int
solveA (Input paper (fold:_)) = Set.size $ doFold fold paper

paperToString :: Paper -> String
paperToString paper = unlines lines
  where
    maxX = maximum $ map fst $ Set.toList paper
    maxY = maximum $ map snd $ Set.toList paper
    lines = [[if Set.member (x, y) paper then '#' else '.' | x <- [0..maxX] ] | y <- [0..maxY]]

solveB :: Input -> String
solveB (Input paper folds) = paperToString $ foldl' (flip doFold) paper folds

main :: IO ()
main = do
  args <- getArgs
  solve :: Input -> IO () <- case args of
    ["A"] -> pure $ print . solveA
    ["B"] -> pure $ putStrLn . solveB
    _ -> putStrLn "bad arguments, defaulting to A" *> pure (print . solveA)
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> solve input
    Left error -> print error
