{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.List (find, transpose)

type Board = [[Int]]
type MarkedBoard = [[(Int, Bool)]]

mark :: Int -> MarkedBoard -> MarkedBoard
mark n = map markRow
  where
    markRow = map (\(m, marked) -> (m, if n == m then True else marked))

check :: MarkedBoard -> Bool
check board = checkRows board || checkRows (transpose board)
  where
    checkRows = any (all snd)

score :: MarkedBoard -> Int -> Int
score board lastNumber = lastNumber * sum (map (sum . map fst . filter (not . snd)) board)

getWinner :: [Board] -> [Int] -> Int
getWinner boards numbers = getWinnerMarked (map initMark boards) numbers
  where
    initMark :: Board -> MarkedBoard
    initMark = map (map (, False))

    getWinnerMarked :: [MarkedBoard] -> [Int] -> Int
    getWinnerMarked _ [] = -1
    getWinnerMarked boards (n : rest) = result
      where
        newBoards = map (mark n) boards
        result = case find check newBoards of
          Just winning -> score winning n
          Nothing -> getWinnerMarked newBoards rest

main :: IO ()
main = do
  numbers <- (\x -> read ("[" ++ x ++ "]") :: [Int]) <$> getLine
  boards <- flip fmap getContents $ \str ->
    let
      numbers = map (read @Int) $ words str
      groupByFive [] = []
      groupByFive xs = let (group, rest) = splitAt 5 xs in group : groupByFive rest
    in groupByFive (groupByFive numbers)
  print $ getWinner boards numbers
