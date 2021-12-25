{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common
  ( module Control.Applicative
  , module Control.Monad
  , module Data.Functor
  , module Text.Parsec

  , Parser
  , intParser
  , digitParser
  , Printable (..)
  , mainImpl
  , unique
  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Functor (($>), void)
import System.Environment (getArgs)
import Text.Parsec hiding (State, (<|>))

type Parser = Parsec String ()

intParser :: Parser Int
intParser = do
  sign <- option "" (string "-")
  digits <- many1 digit
  pure $ read $ sign ++ digits

digitParser :: Parser Int
digitParser = read . (:[]) <$> digit

unique :: Ord a => [a] -> [a]
unique [] = []
unique [x] = [x]
unique (x:y:xs)
  | x == y = unique (x:xs)
  | otherwise = x : unique (y:xs)

class Printable a where
  print' :: a -> IO ()

instance {-# OVERLAPPING #-} Printable String where
  print' = putStr

instance {-# OVERLAPPING #-} Printable (IO ()) where
  print' io = io

instance Show a => Printable a where
  print' = print

mainImpl
  :: (Printable b, Printable c)
  => Parser a
  -> (a -> b)
  -> (a -> c)
  -> IO ()
mainImpl parser solveA solveB = do
  args <- getArgs
  solve <- case args of
    ["A"] -> pure $ print' . solveA
    ["B"] -> pure $ print' . solveB
    _ -> putStrLn "bad arguments, defaulting to A" *> pure (print' . solveA)
  parsedInput <- parse parser "" <$> getContents
  case parsedInput of
    Right input -> solve input
    Left error -> print error
