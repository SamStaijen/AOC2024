{-# LANGUAGE ScopedTypeVariables #-}
module DaySix where
import DayOne ()
import DayTwo ()
import DayThree ()
import DayFour ()
import DayFive ()
import Text.Regex.Posix ((=~))
import Data.Maybe (catMaybes, mapMaybe)
import Debug.Trace
import Data.List as DL
import Data.Text as DT
import GHC.List (errorEmptyList)

daySixPartOne :: [Char] -> Int
daySixPartOne fileContent =
  let
    lines = DL.lines fileContent
  in 1 + countChar (simulate lines) 'X' --the way this program runs, it halts at the last position, but that does not get an X. Thus counter needs +1

--returns: x,y,lookEnumLikeTime
findArrow :: [[Char]] -> (Int,Int, Int)
findArrow lines = (xPos, yPos,lookEnum)
  where
    yPos = DL.head $
          mapMaybe (elemIndex '^') (DL.transpose lines) ++
          mapMaybe (elemIndex '<') (DL.transpose lines) ++
          mapMaybe (elemIndex '>') (DL.transpose lines) ++
          mapMaybe (elemIndex 'V') (DL.transpose lines) ++ [0] --append a false zero for if it goes out of the screen
    xPos = DL.head $
          mapMaybe (elemIndex '^') lines ++
          mapMaybe (elemIndex '<') lines ++
          mapMaybe (elemIndex '>') lines ++
          mapMaybe (elemIndex 'V') lines ++ [0] --append a false zero for if it goes out of the screen
    lookEnum
      | lines !! yPos !! xPos == '^' = 0
      | lines !! yPos !! xPos == '>' = 3
      | lines !! yPos !! xPos == 'V' = 6
      | lines !! yPos !! xPos == '<' = 9
      | otherwise = -999 -- something went wrong

nextField :: [[Char]] -> Int -> Int -> Int -> Char
nextField lines x y lookDir | lookDir == 0 = lines !! (y-1) !! x
                            | lookDir == 3 = lines !! y !! (x+1)
                            | lookDir == 6 = lines !! (y+1) !! x
                            | lookDir == 9 = lines !! y !! (x-1)
                            | otherwise = '@' --something went wrong

countChar :: [[Char]] -> Char -> Int
countChar lines char = DL.length $ DL.concatMap (DL.filter (==char)) lines

simulate :: [[Char]] -> [[Char]]
simulate lines =  if findArrow lines == (0,0,-999) --if no more player
                  then lines --return
                  else traceShow (findArrow lines) simulate $ step lines --recurse

step :: [[Char]] -> [[Char]]
step lines  | lookDir == 0 = if lookingAt == '#' then  replace2D yPos xPos '>' lines else if (yPos-1) < 1 then xPlaced else replace2D (yPos-1) xPos '^' xPlaced   
            | lookDir == 3 = if lookingAt == '#' then  replace2D yPos xPos 'V' lines else if (xPos+1) >= (DL.length lines-1) then xPlaced else replace2D yPos (xPos+1) '>' xPlaced
            | lookDir == 6 = if lookingAt == '#' then  replace2D yPos xPos '<' lines else if (yPos+1) >= (DL.length lines-1) then xPlaced else replace2D (yPos+1) xPos 'V' xPlaced 
            | lookDir == 9 = if lookingAt == '#' then  replace2D yPos xPos '^' lines else if (xPos-1) < 1 then xPlaced else replace2D yPos (xPos-1) '<' xPlaced
            | otherwise = traceShow "no here" lines --if something goes wrong, just return itself
  where
    (xPos,yPos,lookDir) = findArrow lines
    lookingAt = nextField lines xPos yPos lookDir
    xPlaced = replace2D yPos xPos 'X' lines

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt i newValue (x:xs)
    | i == 0    = newValue : xs
    | otherwise = x : replaceAt (i - 1) newValue xs

replace2D :: Int -> Int -> a -> [[a]] -> [[a]]
replace2D _ _ _ [] = []
replace2D row col newValue (r:rs)
    | (row+1) >= DL.length (r:rs) || (col+1) >= DL.length r || row < 0 || col < 0 = r:rs
    | row == 0  = replaceAt col newValue r : rs
    | otherwise = r : replace2D (row - 1) col newValue rs