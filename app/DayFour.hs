module DayFour where

{-# LANGUAGE ScopedTypeVariables #-}
import DayOne ()
import DayTwo ()
import DayThree ()
import Text.Regex.Posix ((=~))
import Debug.Trace
import Data.List as DL
import Data.Text as DT

dayFourPartOne :: [Char] -> Int
dayFourPartOne fileContent =
  let
    regStrs = DL.map DT.unpack $ split (=='\n') $ pack fileContent
    revStrs' = DL.map DL.reverse regStrs
    transpStrs = DL.transpose regStrs
    revTranspStrs = DL.map DL.reverse transpStrs
    crossLTRB = getDiagonals regStrs
    crossRTLB = getDiagonals revStrs'
    revCrossLTRB = DL.map DL.reverse crossLTRB
    revCrossRTLB = DL.map DL.reverse crossRTLB
    allStrs = regStrs ++ revStrs' ++ transpStrs ++ revTranspStrs ++ crossLTRB ++ crossRTLB ++ revCrossLTRB ++ revCrossRTLB
    matches = countAllOccurrences "XMAS" allStrs
  in
     matches

countSubstring :: String -> String -> Int
countSubstring sub str = DL.length $ DL.filter (sub `DL.isPrefixOf`) (DL.tails str)

countAllOccurrences :: String -> [String] -> Int
countAllOccurrences sub = sum . DL.map (countSubstring sub)

getDiagonals :: [String] -> [String]
getDiagonals arr = DL.map extractDiagonal allDiagonalStarts
  where
    rows = DL.length arr
    cols = DL.length (DL.head arr)

    -- Generate starting points for all diagonals
    allDiagonalStarts = [(r, 0) | r <- [0 .. rows - 1]] ++ [(0, c) | c <- [1 .. cols - 1]]

    -- Extract a diagonal starting from (row, col)
    extractDiagonal (r, c) =
      [arr !! (r + d) !! (c + d) | d <- [0 .. min (rows - r - 1) (cols - c - 1)]]

