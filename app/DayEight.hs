{-# LANGUAGE ScopedTypeVariables #-}
module DayEight where
import DayOne ()
import DayTwo ()
import DayThree ()
import DayFour ()
import DayFive ()
import DaySix ()
import DaySeven ()
import Data.Maybe (mapMaybe)
import Debug.Trace ( traceShow )
import Data.List as DL
import Data.Text as DT
import Control.Monad (replicateM)

dayEightPartOne :: [Char] -> Int
dayEightPartOne fileContent =
  let
    lines = DL.lines fileContent
    uniqueChars = DL.filter (/= '\n') $ DL.filter (/= '.') $ nub fileContent
    listOfTuplesFiltered = DL.map ((\(c, elemas) -> (c, DL.filter (noIllegal) elemas)) . (\(c, elems) -> (c, generateTuples elems)) . (\c -> (c, findChar c lines))) uniqueChars
    noIllegal (x, y) = x >= 0 && y >= 0 && x < (DL.length $ DL.head lines) && y < (DL.length lines)
    updatedStr = placeChar listOfTuplesFiltered lines
  in DL.length $ DL.concatMap (DL.filter (=='#')) updatedStr

findChar :: Char -> [[Char]] -> [(Int, Int)]
findChar t grid =
    [ (row, col)
    | (row, line) <- DL.zip [0..] grid
    , (col, char) <- DL.zip [0..] line
    , char == t
    ]

generateTuples :: [(Int, Int)] -> [(Int, Int)]
generateTuples tuples =
    [ (x - dx, y - dy)
    | (x, y) <- tuples
    , (x2, y2) <- tuples
    , (x, y) /= (x2, y2)
    , let dx = x2 - x
    , let dy = y2 - y
    ] ++
    [ (x2 + dx, y2 + dy)
    | (x, y) <- tuples
    , (x2, y2) <- tuples
    , (x, y) /= (x2, y2)
    , let dx = x2 - x
    , let dy = y2 - y
    ]

placeChar :: [(Char, [(Int, Int)])] -> [String] -> [String]
placeChar [] input = input  -- Base case
placeChar ((c, lst):xs) input =
    placeChar xs (DL.foldl (\acc (x, y) -> replaceCharAt y x acc) input lst)

replaceCharAt :: Int -> Int -> [String] -> [String]
replaceCharAt x y grid =
  let (beforeRow, row:afterRow) = DL.splitAt y grid
      (beforeCol, _:afterCol) = DL.splitAt x row
  in beforeRow ++ (beforeCol ++ '#' : afterCol) : afterRow