module Main where

import DayOne
import DayTwo
import DayThree
import DayFour
import DayFive
import DaySix
import DaySeven
import DayEight


main :: IO ()
main = do
  -- Read the file content
  fileContentDayOne <- readFile "app/inputs/dayOne.txt"
  fileContentDayTwo <- readFile "app/inputs/dayTwo.txt"
  fileContentDayThree <- readFile "app/inputs/dayThree.txt"
  fileContentDayFour <- readFile "app/inputs/dayFour.txt"
  fileContentDayFive <- readFile "app/inputs/dayFive.txt"
  fileContentDaySix <- readFile "app/inputs/daySix.txt"
  fileContentDaySeven <- readFile "app/inputs/daySeven.txt"
  fileContentDayEight <- readFile "app/inputs/dayEight.txt"
  -- Process the file content purely
  --let processedContent = dayOnePartOne fileContentDayOne
  --let processedContent = dayOnePartTwo fileContentDayOne
  --let processedContent = dayTwoPartOne fileContentDayTwo
  --let processedContent = dayTwoPartTwo fileContentDayTwo
  --let processedContent = dayThreePartOne fileContentDayThree
  --let processedContent = dayThreePartTwo fileContentDayThree
  --let processedContent = dayFourPartOne fileContentDayFour
  --let processedContent = dayFourPartTwo fileContentDayFour
  --let processedContent = dayFivePartOne fileContentDayFive
  --let processedContent = dayFivePartTwo fileContentDayFive
  --let processedContent = daySixPartOne fileContentDaySix
  --let processedContent = daySevenPartOne fileContentDaySeven
  let processedContent = dayEightPartOne fileContentDayEight
  -- Print the result
  print processedContent