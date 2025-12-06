module Solutions.Day6
  ( aoc6,
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Control.Applicative ((<|>))
import Text.Parser.Char (char)
import Text.Parser.Combinators (many, manyTill, sepBy, some)
import Text.Trifecta (Parser, TokenParsing (token), integer, integer', newline, space, whiteSpace)
import Control.Applicative.Combinators (manyTill_)
import Data.List (transpose)

aoc6 :: IO ()
aoc6 = do
  printSolutions 6 $ MkAoCSolution parseInput part1

-- printSolutions 6 $ MkAoCSolution parseInput part2

parseInput :: Parser ([[Integer]], [Char])
parseInput = manyTill_ (token parseNumbers) parseOperations

parseNumbers :: Parser [Integer]
parseNumbers = many parseInteger

parseInteger :: Parser Integer
parseInteger = do
  many $ char ' '
  n <- integer'
  many $ char ' '
  pure n

parseOperations :: Parser [Char]
parseOperations = some parseOperation

parseOperation :: Parser Char
parseOperation = do
  many $ char ' '
  op <- char '+' <|> char '*'
  many $ char ' '
  pure op

part1 :: Num a => ([[a]], [Char]) -> a
part1 (nums, ops) = sum $ zipWith (curry apply) (transpose nums) ops
  where apply (xs, op) = case op of
          '+' -> sum xs
          '*' -> product xs

part2 :: String -> String
part2 = undefined
