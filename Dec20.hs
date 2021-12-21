import Common
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Vector.Unboxed as V

data Algorithm = Algorithm { getAlgorithm :: V.Vector Bool }
  deriving (Eq, Show)

data Image = Image
  { iImage :: V.Vector Bool
  , iWidth :: Int
  , iHeight :: Int
  , iShiftX :: Int
  , iShiftY :: Int
  , iSurroundingPixels :: Bool
  }
  deriving (Eq, Show)

data Input = Input Algorithm Image
  deriving (Eq, Show)

pixelParser :: Parser Bool
pixelParser = string "#" $> True <|> string "." $> False

algorithmParser :: Parser Algorithm
algorithmParser = Algorithm . V.fromList <$> many pixelParser

imageParser :: Parser Image
imageParser = convert . filter (not . null) <$> sepEndBy (many pixelParser) newline
  where
    convert m = Image
      { iImage = V.fromList $ concat m
      , iWidth = length m
      , iHeight = length $ head m
      , iShiftX = 0
      , iShiftY = 0
      , iSurroundingPixels = False
      }

getPhysicalAddress :: Int -> Int -> Image -> Int
getPhysicalAddress x y i = (x - iShiftX i) + iWidth i * (y - iShiftY i)

getPixel :: Int -> Int -> Image -> Bool
getPixel x y i
  | x < iShiftX i || y < iShiftY i ||
    x - iShiftX i >= iWidth i || y - iShiftY i >= iHeight i = iSurroundingPixels i
  | otherwise = iImage i V.! getPhysicalAddress x y i

toDecimal :: [Bool] -> Int
toDecimal = go 0
  where
    go n [] = n
    go n (x:xs) = go (n * 2 + if x then 1 else 0) xs

applyAlgorithm :: Algorithm -> Image -> Image
applyAlgorithm algorithm i = Image
  { iImage = V.fromList $ do
    y <- [iShiftY i - 2 .. iShiftY i - 2 + iHeight i + 3]
    x <- [iShiftX i - 2 .. iShiftX i - 2 + iWidth i + 3]
    let index = toDecimal $ do
          dy <- [-1..1]
          dx <- [-1..1]
          pure $ getPixel (x + dx) (y + dy) i
    pure $ getAlgorithm algorithm V.! index
  , iWidth = iWidth i + 4
  , iHeight = iHeight i + 4
  , iShiftX = iShiftX i - 2
  , iShiftY = iShiftY i - 2
  , iSurroundingPixels = (if iSurroundingPixels i then V.last else V.head) $ getAlgorithm algorithm
  }

parser :: Parser Input
parser = Input <$> algorithmParser <* newline <* newline <*> imageParser

applyNAndCount :: Int -> Input -> Int
applyNAndCount 0 (Input _ i) = V.sum $ V.map (\b -> if b then 1 else 0) $ iImage i
applyNAndCount n (Input a i) = applyNAndCount (n - 1) (Input a (applyAlgorithm a i))

solveA :: Input -> Int
solveA = applyNAndCount 2

solveB :: Input -> Int
solveB = applyNAndCount 50

main :: IO ()
main = mainImpl parser solveA solveB
