module Main where

import DayOne
import DayTwo
import DayThree

main :: IO ()
main = do
  -- Read the file content
  fileContentDayOne <- readFile "app/inputs/dayOne.txt"
  fileContentDayTwo <- readFile "app/inputs/dayTwo.txt"
  fileContentDayThree <- readFile "app/inputs/dayThree.txt"
  -- Process the file content purely
  --let processedContent = dayOnePartOne fileContentDayOne
  --let processedContent = dayOnePartTwo fileContentDayOne
  --let processedContent = dayTwoPartOne fileContentDayTwo
  --let processedContent = dayTwoPartTwo fileContentDayTwo
  let processedContent = dayThreePartOne fileContentDayThree
  -- Print the result
  print processedContent