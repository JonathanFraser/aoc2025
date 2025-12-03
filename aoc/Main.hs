import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3

import Options.Applicative
import Control.Applicative()

data Day = Day1 | Day2 | Day3 deriving (Show, Eq)

day1Input :: Parser Day
day1Input = flag' Day1
  (  long "day1"
  <> short '1'
  <> help "Run Day1" )

day2Input :: Parser Day
day2Input = flag' Day2
    (  long "day2"
    <> short '2'
    <> help "Run Day2" )

day3Input :: Parser Day
day3Input = flag' Day3
    (  long "day3"
    <> short '3'
    <> help "Run Day3" )

allInput :: Parser Day
allInput = day1Input <|> day2Input <|> day3Input

main :: IO ()
main = do
    day <- execParser $ info (allInput <**> helper)
        ( fullDesc
       <> progDesc "Run Advent of Code 2025 solutions"
       <> header "Advent of Code 2025" )
    case day of
        Day1 -> D1.run_parts
        Day2 -> D2.run_parts
        Day3 -> D3.run_parts
