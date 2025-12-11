import Test.Hspec
import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3
import qualified Day4 as Day4
import qualified Day5 as Day5
import qualified Day6 as Day6
import qualified Day7 as Day7
import qualified Day8 as Day8
import qualified Day9 as Day9
import qualified Day10 as Day10
import qualified Day11 as Day11
import qualified Day12 as Day12

main :: IO ()
main = hspec $ do
    describe "Day1" $ do
        it "part1" $ do
            Day1.part1 `shouldBe` (1011 :: Int)
        it "part2" $
            Day1.part2 `shouldBe` (5937 :: Int)

    describe "Day2" $ do
        it "part1" $ do
            Day2.part1 `shouldBe` (18700015741 :: Int)
        it "part2" $
            Day2.part2 `shouldBe` (20077272987 :: Int)

    describe "Day3" $ do
        it "part1" $ do
            Day3.part1 `shouldBe` (17142 :: Int)
        it "part2" $
            Day3.part2 `shouldBe` (169935154100102 :: Int)

    describe "Day4" $ do
        it "part1" $ do
            Day4.part1 `shouldBe` (1449 :: Int)
        it "part2" $
            Day4.part2 `shouldBe` (8746 :: Int)

    describe "Day5" $ do
        it "part1" $ do
            Day5.part1 `shouldBe` (563 :: Int)
        it "part2" $
            Day5.part2 `shouldBe` (338693411431456 :: Integer)

    describe "Day6" $ do
        it "part1" $ do
            Day6.part1 `shouldBe` (5977759036837 :: Int)
        it "part2" $
            Day6.part2 `shouldBe` (9630000828442 :: Int)

    describe "Day7" $ do
        it "part1" $ do
            Day7.part1 `shouldBe` (1573 :: Int)
        it "part2" $
            Day7.part2 `shouldBe` (15093663987272 :: Integer)

    describe "Day8" $ do
        it "part1" $ do
            Day8.part1 `shouldBe` (129564 :: Int)
        it "part2" $
            Day8.part2 `shouldBe` (42047840 :: Integer)

    describe "Day9" $ do
        it "part1" $ do
            Day9.part1 `shouldBe` (4776100539 :: Int)
        it "part2" $
            Day9.part2 `shouldBe` (0 :: Int)

    describe "Day10" $ do
        it "part1" $ do
            Day10.part1 `shouldBe` (509 :: Int)
        it "part2" $ do
            p2 <- Day10.part2
            p2 `shouldBe` (20083 :: Int)

    describe "Day11" $ do
        it "part1" $ do
            Day11.part1 `shouldBe` (0 :: Int)
        it "part2" $
            Day11.part2 `shouldBe` (0 :: Int)

    describe "Day12" $ do
        it "part1" $ do
            Day12.part1 `shouldBe` (0 :: Int)
        it "part2" $
            Day12.part2 `shouldBe` (0 :: Int)