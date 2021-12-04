{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Data.List (find, partition, transpose)

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

getLooser :: [Board] -> [Int] -> Int
getLooser boards numbers = getLooserMarked (map initMark boards) numbers
  where
    initMark :: Board -> MarkedBoard
    initMark = map (map (, False))

    getLooserMarked :: [MarkedBoard] -> [Int] -> Int
    getLooserMarked _ [] = -1
    getLooserMarked boards (n : restNumbers) = result
      where
        newBoards = map (mark n) boards
        result = case partition check newBoards of
          ([loosing], []) -> score loosing n
          (_, restBoards) -> getLooserMarked restBoards restNumbers

main :: IO ()
main = do
  numbers <- (\x -> read ("[" ++ x ++ "]") :: [Int]) <$> getLine
  boards <- flip fmap getContents $ \str ->
    let
      numbers = map (read @Int) $ words str
      groupByFive [] = []
      groupByFive xs = let (group, rest) = splitAt 5 xs in group : groupByFive rest
    in groupByFive (groupByFive numbers)
  print $ getLooser boards numbers
