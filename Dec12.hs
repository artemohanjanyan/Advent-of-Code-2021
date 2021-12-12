import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Data.Maybe (fromJust)
import Data.Monoid (Sum (..))
import System.Environment (getArgs)
import Text.Parsec hiding (State)

import qualified Data.Map as Map
import qualified Data.Set as Set

type InputParser = Parsec String ()

data Cave = Big String | Small String
  deriving (Eq, Ord, Show)

type CaveSystem = Map.Map Cave [Cave]

cave :: InputParser Cave
cave = (Big <$> many1 upper) <|> (Small <$> many1 lower)

edge :: InputParser (Cave, Cave)
edge = (,) <$> cave <* string "-" <*> cave

inputParser :: InputParser CaveSystem
inputParser = (flip execState Map.empty . mapM_ addEdges) <$> sepBy edge newline
  where
    addEdge :: Cave -> Cave -> State CaveSystem ()
    addEdge from to = do
      caveSystem <- get
      let toList = Map.findWithDefault [] from caveSystem
      put $ Map.insert from (to : toList) caveSystem

    addEdges (from, to) = addEdge from to *> addEdge to from

lookup' :: Cave -> CaveSystem -> [Cave]
lookup' cave caveSystem = fromJust $ Map.lookup cave caveSystem

solveA :: CaveSystem -> Int
solveA caveSystem = getSum $ execWriter $ go Set.empty (Small "start")
  where
    lookup'' = flip lookup' caveSystem

    go :: Set.Set Cave -> Cave -> Writer (Sum Int) ()
    go _ (Small "end") = tell (Sum 1)
    go visited cave = do
      let toList = filter (not . flip Set.member visited) $ lookup'' cave
          newVisited = case cave of
            Small _ -> Set.insert cave visited
            _ -> visited
      mapM_ (go newVisited) toList

solveB :: CaveSystem -> Int
solveB caveSystem = getSum $ execWriter $ go False Set.empty (Small "start")
  where
    lookup'' = flip lookup' caveSystem

    go :: Bool -> Set.Set Cave -> Cave -> Writer (Sum Int) ()
    go _ _ (Small "end") = tell (Sum 1)
    go extraVisit visited cave = do
      let (newExtraVisit, newVisited) = case (Set.member cave visited, cave) of
            (False, Small _) -> (extraVisit, Set.insert cave visited)
            (True, Small _) -> (True, visited)
            _ -> (extraVisit, visited)
          canGo to = not (Set.member to visited) || (to /= Small "start" && not newExtraVisit)
          toList = filter canGo $ lookup'' cave
      mapM_ (go newExtraVisit newVisited) toList

main :: IO ()
main = do
  args <- getArgs
  solve <- case args of
    ["A"] -> pure solveA
    ["B"] -> pure solveB
    _ -> putStrLn "bad arguments, defaulting to A" *> pure solveA
  parsedInput <- parse inputParser "" <$> getContents
  case parsedInput of
    Right input -> print $ solve input
    Left error -> print error
