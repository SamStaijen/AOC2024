module Main where

import DayOne
import DayTwo

main :: IO ()
main = do
  -- Read the file content
  fileContentDayOne <- readFile "app/inputs/dayOne.txt"
  fileContentDayTwo <- readFile "app/inputs/dayTwo.txt"
  -- Process the file content purely
  --let processedContent = dayOnePartOne fileContentDayOne
  --let processedContent = dayOnePartTwo fileContentDayOne
  let processedContent = dayTwoPartOne fileContentDayTwo
  -- Print the result
  print processedContent