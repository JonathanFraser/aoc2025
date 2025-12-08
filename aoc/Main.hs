import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Day7 as D7
import qualified Day8 as D8
import qualified Day9 as D9
import qualified Day10 as D10
import qualified Day11 as D11
import qualified Day12 as D12


import Options.Applicative
import Control.Applicative()

data Day = Day1 | Day2 | Day3 | Day4 | Day5 | Day6 | Day7 | Day8 | Day9 | Day10 | Day11 | Day12 deriving (Show, Eq)

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

day4Input :: Parser Day
day4Input = flag' Day4
    (  long "day4"
    <> short '4'
    <> help "Run Day4" )

day5Input :: Parser Day
day5Input = flag' Day5
    (  long "day5"
    <> short '5'
    <> help "Run Day5" )

day6Input :: Parser Day
day6Input = flag' Day6
    (  long "day6"
    <> short '6'
    <> help "Run Day6" )

day7Input :: Parser Day
day7Input = flag' Day7
    (  long "day7"
    <> short '7'
    <> help "Run Day7" )

day8Input :: Parser Day
day8Input = flag' Day8
    (  long "day8"
    <> short '8'
    <> help "Run Day8" )

day9Input :: Parser Day
day9Input = flag' Day9
    (  long "day9"
    <> short '9'
    <> help "Run Day9" ) 

day10Input :: Parser Day
day10Input = flag' Day10
    (  long "day10"
    <> short 'a'
    <> help "Run Day10" )

day11Input :: Parser Day
day11Input = flag' Day11
    (  long "day11"
    <> short 'b'     
    <> help "Run Day11" )

day12Input :: Parser Day
day12Input = flag' Day12
    (  long "day12"
    <> short 'c'
    <> help "Run Day12" )


allInput :: Parser Day
allInput = day1Input <|> day2Input <|> day3Input <|> day4Input <|> day5Input <|> day6Input <|> day7Input <|> day8Input <|> day9Input <|> day10Input <|> day11Input <|> day12Input

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
        Day4 -> D4.run_parts
        Day5 -> D5.run_parts
        Day6 -> D6.run_parts
        Day7 -> D7.run_parts
        Day8 -> D8.run_parts
        Day9 -> D9.run_parts
        Day10 -> D10.run_parts
        Day11 -> D11.run_parts
        Day12 -> D12.run_parts

