{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day8
  ( aoc8,
  )
where

import Combinatorics (tuples, variate)
import Common.AoCSolutions
  ( AoCSolution (MkAoCSolution),
    printSolutions,
    printTestSolutions,
  )
import Common.FunctorUtils (fmap2, fmap3)
import Control.Lens
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import Linear (Metric (distance))
import Linear.V3 (V3 (V3))
import Text.Parser.Combinators (some)
import Text.Parser.Token (commaSep, integer)
import Text.Printf (printf)
import Text.Trifecta (Parser)

-- Map of node to circuit id
type CircuitMap = Map (V3 Integer) Integer

data CircuitMapState = MkCircuitMapState
  { _nextCircuitId :: Integer, -- We want to increment the circuit number ourselves whenever we start a new one
    _circuitMap :: CircuitMap
  }
  deriving (Eq, Show)

makeLenses ''CircuitMapState

-- Can any new node suddenly join two existing circuits together? Yes, if both nodes are already in different circuits

aoc8 :: IO ()
aoc8 = do
  printTestSolutions 8 $ MkAoCSolution parseInput part1

-- printSolutions 8 $ MkAoCSolution parseInput part2

parseInput :: Parser [V3 Integer]
parseInput = do
  some $ do
    [x, y, z] <- commaSep integer
    pure $ V3 x y z

part1 = buildCircuitMap

-- | Order all pairs of points by their distance from each other, from largest to smallest
orderPairsByDistance :: [V3 Integer] -> [(V3 Integer, V3 Integer)]
orderPairsByDistance points = sortBy (comparing (Down . uncurry distanceF)) allpairs
  where
    allpairs = map (\[x, y] -> (x, y)) $ tuples 2 points
    distanceF :: V3 Integer -> V3 Integer -> Double
    distanceF p1' p2' = distance (fromIntegral <$> p1') (fromIntegral <$> p2')

buildCircuitMap :: [V3 Integer] -> CircuitMapState
buildCircuitMap nodes = foldr processPair initialState orderedPairs
  where
    orderedPairs = orderPairsByDistance nodes
    initialState = MkCircuitMapState 0 M.empty

processPair :: (V3 Integer, V3 Integer) -> CircuitMapState -> CircuitMapState
processPair (p1, p2) state = case (c1, c2) of
  (Nothing, Nothing) ->
    -- Neither point is in a circuit yet, create a new circuit
    -- trace (printf "None connect. P1: %s, P2: %s, state: %s" (show p1) (show p2) (show state)) $
      state
        & nextCircuitId %~ (+ 1)
        & circuitMap
          %~ M.insert p1 (state ^. nextCircuitId)
            . M.insert p2 (state ^. nextCircuitId)
  (Just id1, Just id2) ->
    -- Both points are already in circuits. These two circuits must be merged
    -- ah wait. We need to check the actual ids. If they are the same, we do nothing? Wait isn't that's what is happening anyway?
    -- trace (printf "Both connect. P1: %s, P2: %s, state: %s" (show p1) (show p2) (show state)) $
      state
        & circuitMap %~ M.map (\cid -> if cid == id2 then id1 else cid)
        & circuitMap %~ M.insert p1 id1 . M.insert p2 id1 -- Insert both of these points using id1 (the merge survivor)
  (Just cid, Nothing) ->
    -- p1 is already in a circuit, add p2 to that circuit
    -- trace (printf "1 connects. P1: %s, P2: %s, state: %s" (show p1) (show p2) (show state)) $
      state
        & circuitMap %~ M.insert p2 cid
  (Nothing, Just cid) ->
    -- p2 is already in a circuit, add p1 to that circuit
    -- trace (printf "2 connects. P1: %s, P2: %s, state: %s" (show p1) (show p2) (show state)) $
      state
        & circuitMap %~ M.insert p1 cid
  where
    c1 = M.lookup p1 $ state ^. circuitMap
    c2 = M.lookup p2 $ state ^. circuitMap
    dbgline :: CircuitMapState -> String
    dbgline state = printf "P1: %s, P2: %s, state: %s" (show p1) (show p2) (show state)
