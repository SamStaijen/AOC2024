module DayThree where

import DayOne ()
import DayTwo ()
import Text.Regex.TDFA (getAllTextMatches, (=~), AllTextMatches)
import Debug.Trace

{-
mul (num,num) = multiply 
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(5,5)mul(8,5)) geeft:
mul(2,4) mul(5,5)m mul(5,5), mul(8,5)

idea: grep input based on the following regex : mul\(\d+,\d+\) (take everything that is a valid mul with digit length one or more). After that, parse the digits out of there
-}
dayThreePartOne :: [Char] -> Int
dayThreePartOne fileContent = 
  let
    numTuples =trace (regex) $ extractNumbers fileContent
  in foldr (\(a,b) acc -> acc+(a*b)) 0 numTuples

extractNumbers :: String -> [(Int, Int)]
extractNumbers input =
  let
    regex = "mul\\(([0-9]+),([0-9]+)\\)"
    matches :: [[String]]
    matches = input =~ regex :: [[String]]
  in map (\match -> (read (match !! 1), read (match !! 2))) matches

--idea: parse to 'tokens', which are either mul, or en/disable. Tokens have type (String, Bool) where bool is enableState. Later pattern match on "enable()" _, pr "disable()" _ or on the regex with state True. if enable or disable is found, map over the rest of the list (I want it to be a recursive funtion)
dayThreePartTwo :: [Char] -> Int
dayThreePartTwo fileContent = undefined