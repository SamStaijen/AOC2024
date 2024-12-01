module DayOne where

import Data.List.Split (splitOn)
import Debug.Trace
import Data.List
{-
 ____                   _ 
|  _ \  __ _ _   _     / |
| | | |/ _` | | | |    | |
| |_| | (_| | |_| |    | |
|____/ \__,_|\__, |    |_|
             |___/        

-}
{-
1. read file
2. parse data to 2 lists of Int
3. sort with builtin method, see below
4. make tuples of int,int
5. calculate diff
6. return
-}
-- builtin sort method, haskell loves its math :) 
-- https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-List.html#v:sort
-- Pure function to process file content

-- assumptions:
-- there are an equal amount of numbers in list one and 2
-- both are valid integers, smaller than the Int size in haskell (otherwise: turn in into Integer instead of Int)

--methods will not be made properly, just functioning einough given the assumptions
dayOnePartOne :: [Char] -> Int
dayOnePartOne fileContent = sum (map delta $ toTuples (sortLsts (splLst (words fileContent))))

--splits [a,b,c,d] -> ([a,c],[b,d])
splLst :: [String] -> ([Int],[Int])
splLst [] = ([], [])
splLst [x] = ([read x], [])
splLst (x:y:xs) = (read x : xs1, read y : xs2)
  where
    (xs1, xs2) = splLst xs

sortLsts :: ([Int], [Int]) -> ([Int], [Int])
sortLsts (xs,ys) = (sort xs, sort ys)

toTuples :: ([Int],[Int]) -> [(Int,Int)]
toTuples ([],[]) = []
toTuples ((x:xs), (y:ys)) = (x,y) : toTuples (xs,ys)

delta :: (Int, Int) -> Int
delta (x,y) = if x<y then y-x else x-y

{-
This time, you'll need to figure out exactly how often each number from the left list appears in the right list. 
Calculate a total similarity score by 
  adding up each number in the left list after multiplying it by the number of times that number appears in the right list.
imparative: foreach x in xs, 
-}
dayOnePartTwo :: [Char] -> Int
dayOnePartTwo fileContent =
  let (lst1,lst2) = lstTuple
  in sum $ map (\x -> length (filter (==x) lst2) * x) lst1
  where
    lstTuple = splLst (words fileContent)
    countInLst _ [] acc = acc
    countInLst x (y:ys) acc = if y==x then countInLst x ys (acc+1) else countInLst x ys acc
