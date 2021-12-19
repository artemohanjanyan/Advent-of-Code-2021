import Common
import Control.Applicative
import Control.Monad (guard)

data SNumber = SPair SNumber SNumber | SInt Int
  deriving (Eq, Show)

ppSNumber :: SNumber -> String
ppSNumber (SInt n) = show n
ppSNumber (SPair l r) = "[" ++ ppSNumber l ++ "," ++ ppSNumber r ++ "]"

sNumberParser :: Parser SNumber
sNumberParser =
  string "[" *> (SPair <$> sNumberParser <* string "," <*> sNumberParser) <* string "]" <|>
  SInt <$> intParser

parser :: Parser [SNumber]
parser = sepBy sNumberParser newline

data Direction = L | R
  deriving (Eq, Show)

type Path = [Direction]

findNestedInside4Pairs :: SNumber -> Maybe Path
findNestedInside4Pairs = impl 0
  where
    impl :: Int -> SNumber -> Maybe Path
    impl 4 (SPair _ _) = Just []
    impl n (SPair l r) = (L:) <$> impl (n + 1) l <|> (R:) <$> impl (n + 1) r
    impl _ (SInt _) = Nothing

find10OrHigher :: SNumber -> Maybe Path
find10OrHigher (SInt n) = guard (n >= 10) *> Just []
find10OrHigher (SPair l r) = (L:) <$> find10OrHigher l <|> (R:) <$> find10OrHigher r

findClosestInt :: Direction -> SNumber -> Path -> Maybe Path
findClosestInt dir num path = case dropWhile (== dir) $ reverse path of
  [] -> Nothing
  _ : pathPrefix -> Just $ appendDir num $ reverse $ dir : pathPrefix
  where
    appendDir num [] = goDir num
    appendDir (SPair l _) (L : rest) = L : appendDir l rest
    appendDir (SPair _ r) (R : rest) = R : appendDir r rest
    appendDir (SInt _) _ = error "nonempty path for SInt in appendDir"

    goDir (SInt _) = []
    goDir (SPair l r)
      | dir == L = R : goDir r
      | dir == R = L : goDir l

view :: Path -> SNumber -> SNumber
view [] num = num
view (L:rest) (SPair l _) = view rest l
view (R:rest) (SPair _ r) = view rest r

edit :: (SNumber -> SNumber) -> Path -> SNumber -> SNumber
edit f [] num = f num
edit f (L:rest) (SPair l r) = SPair (edit f rest l) r
edit f (R:rest) (SPair l r) = SPair l (edit f rest r)
edit _ _ _ = error "nonempty path for editing SInt"

explode :: Path -> SNumber -> SNumber
explode path num =
  let SPair (SInt lValue) (SInt rValue) = view path num
  in edit (const (SInt 0)) path $
    maybe id (edit (\(SInt n) -> SInt (n + lValue))) (findClosestInt L num path) $
    maybe id (edit (\(SInt n) -> SInt (n + rValue))) (findClosestInt R num path) num

split :: Path -> SNumber -> SNumber
split path num =
  let SInt n = view path num
  in edit (const $ SPair (SInt $ n `div` 2) (SInt $ (n + 1) `div` 2)) path num

reduce :: SNumber -> SNumber
reduce num = case findNestedInside4Pairs num of
  Just path -> reduce $ explode path num
  Nothing -> case find10OrHigher num of
    Just path -> reduce $ split path num
    Nothing -> num

add :: SNumber -> SNumber -> SNumber
add a b = reduce $ SPair a b

magnitude :: SNumber -> Int
magnitude (SInt n) = n
magnitude (SPair l r) = 3 * magnitude l + 2 * magnitude r

solveA :: [SNumber] -> Int
solveA = magnitude . foldl1 add

solveB :: [SNumber] -> Int
solveB nums = maximum [magnitude (add a b) | a <- nums, b <- nums, a /= b]

main :: IO ()
main = mainImpl parser solveA solveB
