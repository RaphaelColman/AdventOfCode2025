module Solutions.Day5
  ( aoc5,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Data.Range
import Text.Parser.Char
import Text.Trifecta (Parser, integer, some, token, manyTill, integer')
import Control.Monad (void)
import Text.Parser.LookAhead (LookAheadParsing(lookAhead))

aoc5 :: IO ()
aoc5 = do
  --printSolutions 5 $ MkAoCSolution parseInput part1
  printSolutions 5 $ MkAoCSolution parseInput part2

parseInput :: Parser ([Range Integer], [Integer])
parseInput = do
  ranges <- parseRanges
  ingredients <- some integer
  pure (ranges, ingredients)

parseRanges :: Parser [Range Integer]
parseRanges = do
  manyTill parseIngredientRange (newline)

parseIngredientRange :: Parser (Range Integer)
parseIngredientRange = do
  start <- integer'
  char '-'
  end <- integer'
  newline
  pure $ start +=+ end

part1 (ranges, ingredientIds) = length $ filter (inRanges ranges) ingredientIds

part2 (ranges, _) = length $ mergeRanges ranges
--I need to get the size of each range and sum them up.
