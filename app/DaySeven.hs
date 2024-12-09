{-# LANGUAGE ScopedTypeVariables #-}
module DaySeven where
import DayOne ()
import DayTwo ()
import DayThree ()
import DayFour ()
import DayFive ()
import DaySix ()
import Data.Maybe (mapMaybe)
import Debug.Trace ( traceShow )
import Data.List as DL
import Data.Text as DT
import Control.Monad (replicateM)

--for today, part one is also part 2 as i just added an operator
daySevenPartOne :: [Char] -> Int
daySevenPartOne fileContent =
  let
    lines' = DL.lines fileContent
    lines2 = DL.map parseString lines'
    permutations' = DL.map (\(x,ys) -> (x, transform ys)) lines2
  in traceShow permutations' DL.foldr (\(x,y) acc -> acc+x) 0 (DL.filter (\(x,ys) -> x `DL.elem` ys) permutations')


parseString :: String -> (Int, [Int])
parseString str =
  let (numStr, rest) = DL.splitAt (DL.length (DL.takeWhile (/= ':') str)) str
      numbers = DL.map read . DL.words $ DL.drop 1 rest -- drop ":"
  in (read numStr, numbers)

--maybe string concatination wasn't the best idea for an upper limit of 3^(9-1)*850 perms but it works good einough
(&) :: Int -> Int -> Int
(&) x y = read (show x ++ show y)

transform :: [Int] -> [Int]
transform [x] = [x]
transform (x:xs) = DL.foldl apply [x] xs
  where
    apply acc y = DL.map (*y) acc ++ DL.map (+y) acc ++ DL.map (&y) acc
