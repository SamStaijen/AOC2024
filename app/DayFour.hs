{-# LANGUAGE ScopedTypeVariables #-}
module DayFour where
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

--idea: foreach a, take 3x3 grid around it
dayFourPartTwo :: [Char] -> Int
dayFourPartTwo fileContent =
  let
    a3x3 = takeThreeByThree (DL.map DT.unpack $ split (=='\n') $ pack fileContent)
    listOfTrue = DL.filter id $ DL.map followsXRules a3x3
  in traceShow (listOfTrue) $DL.length listOfTrue

followsXRules :: [[Char]] -> Bool
followsXRules strs = DL.concat strs =~ regex
  where
    regex = "M.M.A.S.S|S.S.A.M.M|S.M.A.S.M|M.S.A.M.S"


takeThreeByThree :: [String] -> [[String]]
takeThreeByThree strs =
    let
        -- Get dimensions of the grid
        rows = DL.length strs
        cols = if DL.null strs then 0 else DL.length (DL.head strs)

        -- Find coordinates of all 'A's that are not at the edges
        findAs :: [(Int, Int)]
        findAs = [(i, j) | i <- [1..rows-2], j <- [1..cols-2], (strs !! i !! j) == 'A']

        -- Extract a 3x3 grid centered at (i, j)
        extractGrid :: (Int, Int) -> [String]
        extractGrid (i, j) = DL.map (DL.take 3 . DL.drop (j-1)) (DL.take 3 $ DL.drop (i-1) strs)
    in
        DL.map extractGrid findAs