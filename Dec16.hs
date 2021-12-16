import Control.Monad (guard, replicateM, when)
import Data.Char (isDigit, ord)
import Data.Function (fix)
import Data.List (foldl')
import System.Environment (getArgs)
import Text.Parsec hiding (State, satisfy)

type InputParser = Parsec String ()

newtype HexEncodedPacket = HexEncodedPacket String
  deriving (Show, Eq)

newtype BinaryPacket = BinaryPacket { getBinaryPacket :: [Bool] }
  deriving (Show, Eq)

data Word3 = Word3 Bool Bool Bool
  deriving (Show, Eq)

data Header = Header
  { hVersion :: Word3
  , hTypeId :: Word3
  }
  deriving (Show, Eq)

data Packet = Literal Header [Bool] | Operator Header [Packet]
  deriving (Show, Eq)

inputParser :: InputParser HexEncodedPacket
inputParser = HexEncodedPacket <$> many (digit <|> oneOf "ABCDEF")

toBinaryPacket :: HexEncodedPacket -> BinaryPacket
toBinaryPacket (HexEncodedPacket hex) = BinaryPacket $ concatMap mapDigit hex
  where
    mapDigit = padTo4 . intToBools . hexToInt

    hexToInt c
      | isDigit c = ord c - ord '0'
      | any (== c) "ABCDEF" = 10 + ord c - ord 'A'

    intToBools 0 = []
    intToBools n = intToBools (n `div` 2) ++ [n `mod` 2 == 1]

    padTo4 bools = replicate (4 - length bools) False ++ bools

type BinaryParser = Parsec [Bool] ()

satisfy :: (Bool -> Bool) -> BinaryParser Bool
satisfy pred = tokenPrim show (\p _ _ -> incSourceColumn p 1) (\x -> guard (pred x) *> Just x)

boolParser :: BinaryParser Bool
boolParser = satisfy (const True)

zeroParser :: BinaryParser Bool
zeroParser = satisfy (== False)

oneParser :: BinaryParser Bool
oneParser = satisfy (== True)

outermostPacketParser :: BinaryParser Packet
outermostPacketParser = packetParser <* many zeroParser

packetParser :: BinaryParser Packet
packetParser = do
  header <- headerParser
  if hTypeId header == Word3 True False False
    then Literal header <$> literalParser
    else Operator header <$> operatorParser

headerParser :: BinaryParser Header
headerParser = Header <$> word3Parser <*> word3Parser

word3Parser :: BinaryParser Word3
word3Parser = Word3 <$> boolParser <*> boolParser <*> boolParser

literalParser :: BinaryParser [Bool]
literalParser = do
  isNotLast <- boolParser
  bits <- replicateM 4 boolParser
  rest <- if isNotLast then literalParser else pure []
  pure $ bits ++ rest

operatorParser :: BinaryParser [Packet]
operatorParser = do
  lengthTypeId <- boolParser
  if lengthTypeId == False
    then do
      totalLength <- bitsToNum <$> replicateM 15 boolParser
      startPosition <- getPosition
      fix $ \rec -> do
        packet <- packetParser
        currentPosition <- getPosition
        rest <- if sourceColumn currentPosition < sourceColumn startPosition + totalLength
          then rec else pure []
        pure $ packet : rest
    else do
      totalLength <- bitsToNum <$> replicateM 11 boolParser
      replicateM totalLength packetParser

bitsToNum :: Num a => [Bool] -> a
bitsToNum = foldl' (\a b -> a * 2 + if b then 1 else 0) 0

intVersion :: Header -> Int
intVersion (Header (Word3 b1 b2 b3) _) = bitsToNum [b1, b2, b3]

sumVersions :: Packet -> Int
sumVersions (Literal header _) = intVersion header
sumVersions (Operator header subPackets) = intVersion header + sum (map sumVersions subPackets)

calculatePacket :: Packet -> Integer
calculatePacket (Literal _ bits) = bitsToNum bits
calculatePacket (Operator header subPackets) = op $ map calculatePacket subPackets
  where
    op = case hTypeId header of
      Word3 False False False -> sum
      Word3 False False True  -> product
      Word3 False True  False -> minimum
      Word3 False True  True  -> maximum
      Word3 True  False True  -> binOp (>)
      Word3 True  True  False -> binOp (<)
      Word3 True  True  True  -> binOp (==)

    binOp op [a, b] = if op a b then 1 else 0
    binOp _ _ = error "binary operator is used incorrectly"

go :: (Packet -> a) -> HexEncodedPacket -> Either ParseError a
go f packet = fmap f $ parse outermostPacketParser "" $ getBinaryPacket $ toBinaryPacket packet

solveA :: HexEncodedPacket -> Either ParseError Int
solveA = go sumVersions

solveB :: HexEncodedPacket -> Either ParseError Integer
solveB = go calculatePacket

main :: IO ()
main = do
  args <- getArgs
  solve <- case args of
    ["A"] -> pure $ print . solveA
    ["B"] -> pure $ print . solveB
    _ -> putStrLn "bad arguments, defaulting to A" *> pure (print . solveA)
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> solve input
    Left error -> print error
