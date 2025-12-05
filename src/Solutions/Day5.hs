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
  ( Bound (Bound),
    Range (SingletonRange, SpanRange),
    inRanges,
    mergeRanges,
    (+=+),
  )
import Text.Parser.Char (CharParsing (char), newline)
import Text.Trifecta (Parser, integer, integer', manyTill, some)

aoc5 :: IO ()
aoc5 = do
  printSolutions 5 $ MkAoCSolution parseInput part1
  printSolutions 5 $ MkAoCSolution parseInput part2

parseInput :: Parser ([Range Integer], [Integer])
parseInput = do
  ranges <- parseRanges
  ingredients <- some integer
  pure (ranges, ingredients)

parseRanges :: Parser [Range Integer]
parseRanges = do
  parseIngredientRange `manyTill` newline

parseIngredientRange :: Parser (Range Integer)
parseIngredientRange = do
  start <- integer' <* char '-'
  end <- integer' <* newline
  pure $ start +=+ end

part1 :: (Ord a) => ([Range a], [a]) -> Int
part1 (ranges, ingredientIds) = length $ filter (inRanges ranges) ingredientIds

part2 :: ([Range Integer], b) -> Integer
part2 = sum . map rangeLength . mergeRanges . fst

rangeLength :: Range Integer -> Integer
rangeLength (SpanRange (Bound lower _) (Bound upper _)) = upper - lower + 1
rangeLength (SingletonRange _) = 1
