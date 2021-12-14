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

type LHSC = (Int, Int)
type PairFreq = Map.Map (LHS, LHSC) Int

applyRulesMap :: InsertionRules -> PairFreq -> PairFreq
applyRulesMap rules pairFreq = foldr applyToPair Map.empty $ Map.toList pairFreq
  where
    applyToPair :: ((LHS, LHSC), Int) -> PairFreq -> PairFreq
    applyToPair (lhs'@(lhs@(lhsA, lhsB), (ca, cb)), count) pairFreq = case Map.lookup lhs rules of
      Just rhs ->
        Map.insertWith (+) ((lhsA, rhs), (ca, 1)) count $
        Map.insertWith (+) ((rhs, lhsB), (1, cb)) count pairFreq
      Nothing -> Map.insertWith (+) lhs' count pairFreq

solveB :: Input -> Int
solveB (Input str rules) = maximum frequencies - minimum frequencies
  where
    pairs = zip (zip str (tail str)) (repeat (1, 1))
    changeHead f (x:xs) = f x : xs
    pairs' =
      reverse $ changeHead (\(p, (a, b)) -> (p, (a, b + 1))) $
      reverse $ changeHead (\(p, (a, b)) -> (p, (a + 1, b))) pairs
    startPairFreq = Map.fromListWith (+) $ zip pairs' (repeat 1)
    resultPairFreq = foldr ($) startPairFreq $ replicate 40 (applyRulesMap rules)

    f (((a, b), (ca, cb)), count) m =
      Map.insertWith (+) a (count * ca) $
      Map.insertWith (+) b (count * cb) m
    letterMap = foldr f Map.empty $ Map.toList resultPairFreq

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
