{-# LANGUAGE ScopedTypeVariables #-}

import Data.Functor (($>))
import Data.List (foldl')
import System.Environment (getArgs)
import Text.Parsec hiding (State)

import qualified Data.Map as Map
import qualified Data.Set as Set

type InputParser = Parsec String ()

type Template = String

type LHS = (Char, Char)
type InsertionRule = (LHS, Char)
type InsertionRules = Map.Map LHS Char

data Input = Input Template InsertionRules

templateParser :: InputParser Template
templateParser = many1 upper

insertionRuleParser :: InputParser InsertionRule
insertionRuleParser = (,) <$> ((,) <$> upper <*> upper) <* string " -> " <*> upper

inputParser :: InputParser Input
inputParser = Input <$>
  templateParser <*
  newline <* newline <*>
  (Map.fromList <$> sepBy insertionRuleParser newline)

applyRules :: InsertionRules -> String -> String
applyRules rules str = merge str newChars
  where
    merge :: String -> [Maybe Char] -> String
    merge [] _ = []
    merge xs [] = xs
    merge (x : xs) (Just y : ys) = x : y : merge xs ys
    merge (x : xs) (Nothing : ys) = x : merge xs ys

    pairs = zip str (tail str)
    newChars = map (flip Map.lookup rules) pairs

solveA :: Input -> Int
solveA (Input str rules) = maximum frequencies - minimum frequencies
  where
    resultStr = foldr ($) str $ replicate 10 (applyRules rules)

    count c = length $ filter (== c) resultStr
    frequencies = filter (/= 0) $ map count ['A'..'Z']

type PairFreq = Map.Map LHS Int

applyRulesMap :: InsertionRules -> PairFreq -> PairFreq
applyRulesMap rules pairFreq = foldr applyToPair Map.empty $ Map.toList pairFreq
  where
    applyToPair :: (LHS, Int) -> PairFreq -> PairFreq
    applyToPair (lhs@(lhsA, lhsB), count) pairFreq = case Map.lookup lhs rules of
      Just rhs ->
        Map.insertWith (+) (lhsA, rhs) count $
        Map.insertWith (+) (rhs, lhsB) count pairFreq
      Nothing -> Map.insertWith (+) lhs count pairFreq

solveB :: Input -> Int
solveB (Input str rules) = maximum frequencies - minimum frequencies
  where
    startPairFreq = Map.fromListWith (+) $ zip (zip str (tail str)) (repeat 1)
    resultPairFreq = foldr ($) startPairFreq $ replicate 40 (applyRulesMap rules)

    f ((a, b), count) m =
      Map.insertWith (+) a count $
      Map.insertWith (+) b count m
    letterMap =
      Map.insertWith (+) (head str) 1 $
      Map.insertWith (+) (last str) 1 $
      foldr f Map.empty $ Map.toList resultPairFreq

    count c = Map.findWithDefault 0 c letterMap `div` 2
    frequencies = filter (/= 0) $ map count ['A'..'Z']

main :: IO ()
main = do
  args <- getArgs
  solve :: Input -> IO () <- case args of
    ["A"] -> pure $ print . solveA
    ["B"] -> pure $ print . solveB
    _ -> putStrLn "bad arguments, defaulting to A" *> pure (print . solveA)
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> solve input
    Left error -> print error
