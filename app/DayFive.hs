{-# LANGUAGE ScopedTypeVariables #-}
module DayFive where
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
    (rules, pagesPlusEmpty) = DL.break (== "") $ DL.map DT.unpack $ split (=='\n') $ pack fileContent
    legalPages = DL.filter
                (\pgs -> not $ mapInPrevious [] pgs (DL.map rulesToTuples rules))
                (DL.map stringToIntList (DL.drop 1 pagesPlusEmpty))
  in DL.foldr ((+) . getMiddle) 0 legalPages

getMiddle :: [a] -> a
getMiddle lst = lst !! (DL.length lst `div` 2)

rulesToTuples :: String -> (Int, Int)
rulesToTuples str =
    let regex = "([0-9]+)\\|([0-9]+)"
        match = str =~ regex :: [[String]]
    in case match of
        [[_, num1, num2]] -> (read num1, read num2)
        _ -> error $ "Invalid format: " ++ str

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

dayFivePartTwo :: [Char] -> Int
dayFivePartTwo fileContent =
  let
    (rules, pagesPlusEmpty) = DL.break (== "") $ DL.map DT.unpack $ split (=='\n') $ pack fileContent
    illegalPages = DL.filter
                (\pgs ->  mapInPrevious [] pgs (DL.map rulesToTuples rules))
                (DL.map stringToIntList (DL.drop 1 pagesPlusEmpty))
    sortedIllegals = DL.map (\illgl -> makeLegal illgl (DL.map rulesToTuples rules)) illegalPages
  in
    DL.foldr ((+) . getMiddle) 0 sortedIllegals

makeLegal :: [Int] -> [(Int,Int)] -> [Int]
makeLegal xs rules = sortBy (compareRules rules) xs

compareRules :: [(Int,Int)] -> Int -> Int -> Ordering
compareRules rules a b
  | (a, b) `DL.elem` rules = LT
  | (b, a) `DL.elem` rules = GT 
  | otherwise = EQ