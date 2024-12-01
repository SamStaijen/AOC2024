module Main where

import DayOne
main :: IO ()
main = do
  -- Read the file content
  fileContentDayOne <- readFile "app/inputs/dayOne.txt"
  -- Process the file content purely
  --let processedContent = dayOnePartOne fileContentDayOne
  let processedContent = dayOnePartTwo fileContentDayOne
  -- Print the result
  print processedContent