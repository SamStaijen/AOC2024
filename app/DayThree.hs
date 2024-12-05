{-# LANGUAGE ScopedTypeVariables #-}
module DayThree where
import DayOne ()
import DayTwo ()
import Text.Regex.Posix ((=~))
import Debug.Trace
import Data.List

{-
mul (num,num) = multiply 
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(5,5)mul(8,5)) geeft:
mul(2,4) mul(5,5)m mul(5,5), mul(8,5)

idea: grep input based on the following regex : mul\(\d+,\d+\) (take everything that is a valid mul with digit length one or more). After that, parse the digits out of there
-}
dayThreePartOne :: [Char] -> Int
dayThreePartOne fileContent =
  let
    numTuples = extractNumbers fileContent
  in foldr (\(a,b) acc -> acc+(a*b)) 0 numTuples

extractNumbers :: String -> [(Int, Int)]
extractNumbers input =
  let
    regex = "mul\\(([0-9]+),([0-9]+)\\)"
    matches :: [[String]]
    matches = input =~ regex :: [[String]]
  in map (\match -> (read (match !! 1), read (match !! 2))) matches

--answer before refractor: 79845780
dayThreePartTwo :: [Char] -> Int
dayThreePartTwo fileContent =
  let
    tokens = extractThingies fileContent
    func (val,state) (Mul (a,b)) = if state then (val+(a*b),state) else (val,state)
    func (val,_) Enable  = (val,True)
    func (val,_) Disable  = (val,False)
    theAcc :: (Int, Bool)
    theAcc = (0, True)
  in fst (foldl func theAcc tokens)


-- Data type for tokens
data Token = Mul (Int, Int) | Enable | Disable
    deriving (Show, Eq)

extractThingies :: String -> [Token]
extractThingies input =
  let
    -- Define regex patterns
    regex = "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)"

    -- Extract matches for each pattern
    extractMatches :: String -> String -> [[String]]
    extractMatches inp regex = inp =~ regex

    -- Convert matches to Tokens
    parse :: [String] -> Token
    parse [match, c1, c2]
      | isPrefixOf "mul" match = Mul (read (c1), read (c2))
      | isPrefixOf "don" match = Disable
      | isPrefixOf "do" match = Enable
      | otherwise = error "whoopsie"

    -- Collect all tokens from all patterns
    tokens = map parse (extractMatches input regex)

  in tokens