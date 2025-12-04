module Solutions.Day4
  ( aoc4,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.Geometry (Grid, Point, enumerateMultilineStringToVectorMap, gridNeighbours)
import Control.Applicative.Combinators (some)
import Control.Monad.Reader (MonadReader (ask), Reader, filterM, runReader)
import qualified Data.Map.Strict as M
import Debug.Trace
import Text.Trifecta (CharParsing (anyChar), Parser)

aoc4 :: IO ()
aoc4 = do
  printSolutions 4 $ MkAoCSolution parseInput part1

-- printSolutions 4 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  allChars <- some anyChar
  pure $ enumerateMultilineStringToVectorMap allChars

part1 = runReader solve

part2 :: String -> String
part2 = undefined

accessible :: Point -> Reader (Grid Char) Bool
accessible point = do
  grid <- ask
  let neighbours = gridNeighbours grid point
  let neighbourRolls = filter (== '@') $ M.elems neighbours
  pure $ length neighbourRolls < 4 && grid M.! point == '@'

solve :: Reader (Grid Char) Integer
solve = do
  grid <- ask
  let points = M.keys grid
  accessiblePoints <- filterM accessible points
  pure $ toInteger $ length accessiblePoints
