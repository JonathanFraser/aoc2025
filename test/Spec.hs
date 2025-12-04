import Test.Hspec
import qualified Day1 as Day1
import qualified Day2 as Day2
import qualified Day3 as Day3
import qualified Day4 as Day4

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