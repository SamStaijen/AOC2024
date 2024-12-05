module DayFive where

{-# LANGUAGE ScopedTypeVariables #-}
import DayOne ()
import DayTwo ()
import DayThree ()
import DayFour ()
import Text.Regex.Posix ((=~))
import Debug.Trace
import Data.List as DL
import Data.Text as DT

{-
X|Y is x before y

assume
1,2,3
5,2,6
are both correct, 
then take middle
2 and 2
that is 4

answer is four
-}
dayFivePartOne :: [Char] -> Int
dayFivePartOne fileContent =
  let
    x = DL.map DT.unpack $ split (=='\n') $ pack fileContent
    (rules, pagesPlusEmpty) = DL.break (== "") x
    pages = DL.drop 1 pagesPlusEmpty
    rulesToTuples' = DL.map rulesToTuples rules
    pagesToArrays :: [[Int]]
    pagesToArrays = DL.map stringToIntList pages
    legalPages = DL.filter (\pgs -> not $ mapInPrevious [] pgs rulesToTuples') pagesToArrays
    getMiddlenum lst = lst !! (DL.length lst `div` 2)
    sumMiddleNum = DL.foldr ((+) . getMiddlenum) 0 legalPages
  in sumMiddleNum

rulesToTuples :: String -> (Int, Int)
rulesToTuples str =
    let regex = "([0-9]+)\\|([0-9]+)" -- Regex to match numbers separated by a pipe
        match = str =~ regex :: [[String]] -- Perform the regex match
    in case match of
        [[_, num1, num2]] -> (read num1, read num2) -- Convert captured strings to integers
        _ -> error $ "Invalid format: " ++ str -- Handle unexpected input format

stringToIntList :: String -> [Int]
stringToIntList str =
    let textValues = splitOn (pack ",") (pack str)
    in Prelude.map (read . unpack) textValues

--if false then okey
mapInPrevious :: [Int] -> [Int] -> [(Int,Int)] -> Bool
mapInPrevious prevs [] rules = False
mapInPrevious prevs (x:xs) rules = inPrevious prevs x rules || mapInPrevious (x:prevs) xs rules
--if this is false, the rules hold
inPrevious :: [Int] -> Int -> [(Int,Int)] -> Bool
inPrevious prevs x rules = not $ DL.null filtredList
  where
    --als het eerste element gelijk is aan x, dan mag het tweede element niet in de voorgaande zitten
    filtredList = DL.filter (\(y,z) -> x==y && z `DL.elem` prevs) rules