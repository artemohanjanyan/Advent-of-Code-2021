import Common
import Control.Monad (forM)
import Control.Monad.Trans.State.Strict
import qualified Data.Map as Map

parser :: Parser (Int, Int)
parser = (,) <$>
  (string "Player 1 starting position: " *> intParser <* newline) <*>
  (string "Player 2 starting position: " *> intParser <* optional newline)

a +% b = (a + b - 1) `mod` 10 + 1
a -% b = (a - b - 1 + 20) `mod` 10 + 1

rollUntil1000 :: Int -> Int -> Int -> Int -> Int -> [Int] -> Int
rollUntil1000 score1 score2 pos1 pos2 rollN dices = let
  a:b:c:dices' = dices
  pos1' = pos1 +% a +% b +% c
  score1' = score1 + pos1'
  rollN' = rollN + 3
  in if score1' >= 1000 then score2 * rollN' else let
    d:e:f:dices'' = dices'
    pos2' = pos2 +% d +% e +% f
    score2' = score2 + pos2'
    rollN'' = rollN' + 3
    in if score2' >= 1000 then score1 * rollN'' else
      rollUntil1000 score1' score2' pos1' pos2' rollN'' dices''

solveA :: (Int, Int) -> Int
solveA (pos1, pos2) = rollUntil1000 0 0 pos1 pos2 0 (concat $ repeat [1..100])

type St = (Int, Int, Int, Int, Bool)

minWinningScore :: Int
minWinningScore = 21

throwFrequencies :: [(Int, Int)]
throwFrequencies = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

calc :: St -> State (Map.Map St Int) Int
calc st@(score1, score2, pos1, pos2, fl)
  | score1 < 0 || score2 < 0 ||
    (fl && score1 - pos1 >= minWinningScore) ||
    (not fl && score2 - pos2 >= minWinningScore) = pure 0
calc st@(score1, score2, pos1, pos2, fl) = do
  cashed <- gets (Map.lookup st)
  case cashed of
    Just result -> pure result
    Nothing -> do
      let score1' = if fl then score1 - pos1 else score1
          score2' = if not fl then score2 - pos2 else score2
          pos1f n = if fl then pos1 -% n else pos1
          pos2f n = if not fl then pos2 -% n else pos2
      all <- forM throwFrequencies $ \(throwResult, frequency) ->
        (* frequency) <$> calc (score1', score2', pos1f throwResult, pos2f throwResult, not fl)
      let result = sum all
      modify (Map.insert st result)
      pure result

calcWins :: State (Map.Map St Int) (Int, Int)
calcWins = do
  all <- forM [minWinningScore..minWinningScore + 9] $ \winningScore ->
    forM [0..minWinningScore - 1] $ \losingScore ->
      forM [1..10] $ \pos1 ->
        forM [1..10] $ \pos2 -> do
          a <- calc (winningScore, losingScore, pos1, pos2, True)
          b <- calc (losingScore, winningScore, pos1, pos2, False)
          pure (a, b)
  let (a, b) = unzip $ concat $ concat $ concat all
  pure (sum a, sum b)

solveB :: (Int, Int) -> Int
solveB (pos1, pos2) = uncurry max $ evalState calcWins (Map.singleton (0, 0, pos1, pos2, False) 1)
--solveB (pos1, pos2) = let (result, mem) = runState calcWins (Map.singleton (0, 0, pos1, pos2, False) 1)
--  in do
--    print result
--    mapM_ print $ Map.toList $ Map.filter (/= 0) mem

main :: IO ()
main = mainImpl parser solveA solveB
