import Common

import Control.Monad.ST (runST)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

data Seafloor = Seafloor
  { sVector :: V.Vector Word8
  , sWidth :: Int
  , sHeight :: Int
  }
  deriving (Eq, Show)

locationParser :: Parser Word8
locationParser = 
  string "v" $> 1 <|>
  string ">" $> 2 <|>
  string "." $> 3

parser :: Parser Seafloor
parser = do
  locations <- sepEndBy (many locationParser) newline
  pure Seafloor
    { sVector = V.fromList $ concat $ locations
    , sWidth = length $ head locations
    , sHeight = length locations
    }

getPhysicalAddress :: Seafloor -> Int -> Int -> Int
getPhysicalAddress seafloor x y =
  (x `mod` sWidth seafloor) + (y `mod` sHeight seafloor) * sWidth seafloor

move :: Int -> Int -> Word8 -> Seafloor -> Seafloor
move dx dy target seafloor = seafloor
  { sVector = runST $ do
    vector <- MV.replicate (sWidth seafloor * sHeight seafloor) 0
    forM_ [0..sWidth seafloor - 1] $ \x ->
      forM_ [0..sHeight seafloor - 1] $ \y ->
        if sVector seafloor V.! addr x y == target &&
          sVector seafloor V.! addr (x + dx) (y + dy) == 3
          then do
            MV.write vector (addr x y) 3
            MV.write vector (addr (x + dx) (y + dy)) target
          else do
            current <- MV.read vector (addr x y)
            when (current == 0) $
              MV.write vector (addr x y) (sVector seafloor V.! addr x y)
    V.unsafeFreeze vector
  }
  where
    addr = getPhysicalAddress seafloor

solveA :: Seafloor -> Int
solveA seafloor =
  let next = move 0 1 1 $ move 1 0 2 seafloor
  in if next == seafloor then 1 else 1 + solveA next

main :: IO ()
main = mainImpl parser solveA print
