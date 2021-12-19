{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common
  ( module Text.Parsec
  , Parser
  , intParser
  , digitParser
  , Printable (..)
  , mainImpl
  ) where

import System.Environment (getArgs)
import Text.Parsec hiding (State, (<|>))

type Parser = Parsec String ()

intParser :: Parser Int
intParser = read <$> many1 digit

digitParser :: Parser Int
digitParser = read . (:[]) <$> digit

class Printable a where
  print' :: a -> IO ()

instance {-# OVERLAPPING #-} Printable String where
  print' = putStr

instance Show a => Printable a where
  print' = print

instance Printable (IO ()) where
  print' = id

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
