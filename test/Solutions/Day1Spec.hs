module Solutions.Day1Spec (spec) where

import Test.Hspec
import Solutions.Day1

spec :: Spec
spec = do
  describe "Part 2" $ do
    it "Rotating right to 0 should increment" $ do
      part2 [50] `shouldBe` 1
    it "Rotating left to 0 should increment" $ do
      part2 [-50] `shouldBe` 1
    it "Rotating past 0 should increment" $ do
      part2 [51] `shouldBe` 1
    it "Rotating left past 0 should increment" $ do
      part2 [-51] `shouldBe` 1
    it "Rotating right 3 rotations should increment" $ do
      part2 [250] `shouldBe` 3
    it "Rotating right over 3 rotations should increment" $ do
      part2 [251] `shouldBe` 3
    it "Rotating left 3 rotations should increment" $ do
      part2 [-250] `shouldBe` 3
    it "Rotating left over 3 rotations should increment" $ do
      part2 [-251] `shouldBe` 3
    it "Rotating left to exactly 0 then left again is still 1 0" $ do
      part2 [-50, -1] `shouldBe` 1
