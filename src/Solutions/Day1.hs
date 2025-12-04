{-# LANGUAGE DataKinds #-}

module Solutions.Day1
  ( aoc1,
    part2
  )
where

import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Control.Applicative ((<|>))
import Data.Foldable (Foldable (foldl'))
import Data.List (tails)
import Data.Range
import Text.Trifecta
  ( CharParsing (anyChar, char),
    Parser,
    TokenParsing (token),
    integer,
    some,
  )

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = some parseInstr

parseInstr :: Parser Integer
parseInstr = do
  c <- char 'L' <|> char 'R'
  i <- integer
  case c of
    'L' -> pure (-i)
    'R' -> pure i
    _ -> error "unrecognized direction"

part1 = length . filter (== 0) . scanl (\b a -> (b + a) `mod` 100) 50

part2 = fst .
  foldl'
    ( \(count, dialValue) x ->
        let (c, newValue) = (dialValue + x) `divMod` 100
            accom = if newValue == 0 && x < 0 then 1 else 0  -- Turning left to exactly 0 should count as a click
            lFromZero = if dialValue == 0 && x < 0 then -1 else 0 -- Turning left from 0 should not count as a click
         in (count + abs c + accom + lFromZero, newValue)
    )
    (0, 50)
