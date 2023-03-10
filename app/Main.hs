module Main (main) where

import Control.Parallel.Strategies (parList, parMap, rdeepseq, using)
import Data.List (nub)
import GHC.Conc (par)
import Lib
import ParFilter
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

maxRange :: Int -> Float
maxRange n = fromIntegral n / 4.0

maxDistance :: Int -> Float
maxDistance n = fromIntegral n / 2.0

readCsv :: String -> [[Int]]
readCsv csvContent = [[read word | word <- words line] | line <- lines csvContent]

findHits :: [Int] -> [Int]
findHits xs = parMap rdeepseq snd $ filter ((> 0) . fst) (zip xs [0 ..])

getDistance :: [Int] -> Int -> Int
getDistance [] _ = 0
getDistance (hpath : tpath@(headtpath:_)) n
  | not $ null tpath = distance hpath (headtpath) n + getDistance tpath n
  | otherwise = 0
getDistance [_] _ = 0

getTotaldistance :: Int -> [Int] -> Int -> Int
getTotaldistance depth path n = round $ (fromIntegral (getDistance path n) :: Float) / (fromIntegral (depth - 1) :: Float)

getPathlist :: Int -> [[Int]] -> [[Int]] -> Int -> [[Int]]
getPathlist _ [] _ _ = []
getPathlist _ pathlist [] _ = pathlist
getPathlist depth pathlist pixels n =
  concat [getPathlist (depth + 1) (getPath (depth + 1) path hys n) tys n | path <- pathlist, checkPath path] `using` parList rdeepseq
  where
    (hys : tys) = pixels

getPath :: Int -> [Int] -> [Int] -> Int -> [[Int]]
getPath depth path [] _
  | length path == depth = [path]
  | otherwise = []
getPath depth path hys n = [path ++ [hit] | hit <- hitlist (getTotaldistance depth path n) (last path) hys n]

hitlist :: Int -> Int -> [Int] -> Int -> [Int]
hitlist d r hys n = [y | y <- findHits hys, y `elem` getRange r d (round $ maxRange n) n]

putRange :: Int -> Int -> Int
putRange n x
  | x < 0 = n - abs x
  | x > n - 1 = x - n
  | otherwise = x

getRange :: Int -> Int -> Int -> Int -> [Int]
getRange x y range n = map (putRange n) [x + y - range .. x + y + range]

distance :: Int -> Int -> Int -> Int
distance fidx sidx n
  | sidx > fidx && abs (sidx - fidx) < abs (n - sidx + fidx) = sidx - fidx
  | sidx > fidx && abs (sidx - fidx) >= abs (n - sidx + fidx) = n - sidx + fidx
  | sidx < fidx && abs (fidx - sidx) < abs (n - fidx + sidx) = fidx - sidx
  | sidx < fidx && abs (fidx - sidx) >= abs (n - fidx + sidx) = n - fidx + sidx
  | otherwise = 0

diffPath :: [Int] -> [Int]
diffPath path = tail (zipWith (-) path (0 : init path))

checkPath :: [Int] -> Bool
checkPath path = length delem < 3
  where
    delem = nub $ diffPath path

alld :: [Int] -> [Int] -> Int -> [[Int]]
alld firsthits secondhits n =
  concat
    [ [ [firsthit, secondhit]
        | firsthit <- firsthits,
          abs (distance firsthit secondhit n) <= round (maxDistance n)
      ]
      | secondhit <- secondhits
    ]

calc :: [[Int]] -> Int -> String
calc [] _ = ""
calc [[]] _ = ""
calc (hp : htp : ttp) n = concat [tail $ init (show line) ++ "\n" | line <- filtered_tracks]
  where
    tracks = par (findHits (htp)) (getPathlist 2 (alld (findHits hp) (findHits (htp)) n) (ttp) n)
    filtered_tracks = parFilter (isValid n) tracks
calc [(_:_)] _ = ""

main :: IO ()
main = do
  arguments <- getArgs
  file <- openFile (head arguments) ReadMode
  csvContent <- hGetContents file
  let pixels = readCsv csvContent
  let n = length $ head pixels
  putStr $ calc pixels n
