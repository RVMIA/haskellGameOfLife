
module Main where

import Control.Concurrent
import Data.List

stringProc :: [String] -> [[Int]]
stringProc = addToBlank . map (map read . words)

blankScreen :: [[Int]]
blankScreen = replicate 37 $ replicate 87 0

padToInf :: [[Int]] -> [[Int]]
padToInf = flip (++) (repeat $ repeat 0) . map (++ repeat 0) 

addToBlank :: [[Int]] -> [[Int]]
addToBlank a = sum2D blankScreen $ padToInf a

toUnicode :: Int -> Char
toUnicode 0 = '░'
toUnicode _ = '█'

putArr :: ([[Int]], Int) -> IO ()
putArr (a,b) = do
  putStr "\x1B[?25l\x1B[H"
  putStr $ unlines $ map (Prelude.concatMap (replicate 2 . toUnicode)) a
  print b
  putStr "\x1B[?25h"
  threadDelay 1
  -- threadDelay $ div 1000000 fps 

rotate2D :: Int -> Int -> [[Int]] -> [[Int]]
rotate2D x y arr = rot y $ map (rot x) arr
  where
    rot :: (Ord a1, Num a1) => a1 -> [a2] -> [a2]
    rot n a
      | n < 0 = rot (n + 1) (tail a ++ [head a])
      | n > 0 = rot (n - 1) (last a : init a)
      | otherwise = a

sum2D :: (Num a) => [[a]] -> [[a]] -> [[a]]
sum2D = zipWith (zipWith (+))

neighbors :: [[Int]] -> [[Int]]
neighbors a = foldr (sum2D . r) a [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
  where
    r (x, y) = rotate2D x y a


step :: [[Int]] -> [[Int]]
step a = sum2D (three b) (and2D (four b) a)
  where
    b = neighbors a
    three = map $ map $ isN 3
    four = map $ map $ isN 4
    isN n t
      | n == t = 1
      | otherwise = 0
    and2D = zipWith (zipWith and')
      where
        and' :: Int -> Int -> Int
        and' a b
          | a == 0 = 0
          | otherwise = b



-- Sample Patterns
copperHead :: [[Int]]
copperHead = stringProc [
    "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
  , "0 0 0 0 0 0 1 0 1 1 0 0 0 0 0"
  , "0 0 0 0 0 1 0 0 0 0 0 0 1 0 0"
  , "0 0 0 0 1 1 0 0 0 1 0 0 1 0 0"
  , "0 1 1 0 1 0 0 0 0 0 1 1 0 0 0"
  , "0 1 1 0 1 0 0 0 0 0 1 1 0 0 0"
  , "0 0 0 0 1 1 0 0 0 1 0 0 1 0 0"
  , "0 0 0 0 0 1 0 0 0 0 0 0 1 0 0"
  , "0 0 0 0 0 0 1 0 1 1 0 0 0 0 0"
  , "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
  ]


glider :: [[Int]]
glider = stringProc [
    "0 0 0 0 0"
  , "0 0 0 1 0"
  , "0 1 0 1 0"
  , "0 0 1 1 0"
  , "0 0 0 0 0"
  ]

jaydot :: [[Int]]
jaydot  = stringProc [
    "0 0 1 1 0"
  , "0 1 1 1 0"
  , "0 0 0 0 0"
  , "0 0 1 0 0"
  , "0 0 1 1 0"
  , "0 1 0 0 0"
  ]


acorn :: [[Int]]
acorn = stringProc [
    "0 0 0 0 0 0 0 0 0 0"
  , "0 0 0 1 0 0 0 0 0 0"
  , "0 0 0 0 0 1 0 0 0 0"
  , "0 0 1 1 0 0 1 1 1 0"
  , "0 0 0 0 0 0 0 0 0 0"
  ]

dieHard :: [[Int]]
dieHard = stringProc [
    "0 0 0 0 0 0 1 0"
  , "1 1 0 0 0 0 0 0"
  , "0 1 0 0 0 1 1 1"
  ]

pentaDecathalon :: [[Int]]
pentaDecathalon = stringProc [
   "0 0 0 0 0 "
  ,"0 1 1 1 0 "
  ,"0 1 0 1 0 "
  ,"0 1 1 1 0 "
  ,"0 1 1 1 0 "
  ,"0 1 1 1 0 "
  ,"0 1 1 1 0 "
  ,"0 1 0 1 0 "
  ,"0 1 1 1 0 "
  ,"0 0 0 0 0 "
  ]

infinite1 :: [[Int]]
infinite1 = stringProc [
   "0 0 0 0 0 0 0 0 0 0 "
  ,"0 0 0 0 0 0 0 1 0 0 "
  ,"0 0 0 0 0 1 0 1 1 0 "
  ,"0 0 0 0 0 1 0 1 0 0 "
  ,"0 0 0 0 0 1 0 0 0 0 "
  ,"0 0 0 1 0 0 0 0 0 0 "
  ,"0 1 0 1 0 0 0 0 0 0 "
  ,"0 0 0 0 0 0 0 0 0 0 "
  ]
  
infinite2 :: [[Int]]
infinite2 = stringProc [
   "0 0 0 0 0 0 0 "
  ,"0 1 1 1 0 1 0 "
  ,"0 1 0 0 0 0 0 "
  ,"0 0 0 0 1 1 0 "
  ,"0 0 1 1 0 1 0 "
  ,"0 1 0 1 0 1 0 "
  ,"0 0 0 0 0 0 0 "
  ]

rPentomino :: [[Int]]
rPentomino = stringProc [
   "0 1 1"
  ,"1 1 0"
  ,"0 1 0"
  ]
                        

infinite3 :: [[Int]]
infinite3 = stringProc ["1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 0 0 1 1 1 0 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1"]

stillLife19 :: [[Int]]
stillLife19 = stringProc [
   "1 1 0 1 1 0 1 1 0 1 1 0 1 0 1 1 0 1 1 "
  ,"1 1 0 1 1 0 1 1 0 1 0 1 1 1 0 1 0 1 1 "
  ,"0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 "
  ,"1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1 "
  ,"1 1 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0 1 "
  ,"0 0 0 1 0 1 1 0 1 0 0 1 1 1 0 1 0 1 0 "
  ,"1 1 0 1 0 1 0 0 1 1 1 0 0 1 0 1 0 1 1 "
  ,"1 1 0 1 0 0 1 1 0 0 0 1 1 0 0 1 0 1 0 "
  ,"0 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 "
  ,"1 1 1 0 0 0 1 1 0 0 0 1 1 0 0 0 1 1 1 "
  ,"1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 0 "
  ,"0 1 0 1 0 0 1 1 0 0 0 1 1 0 0 1 0 1 1 "
  ,"1 1 0 1 0 1 0 0 1 1 1 0 0 1 0 1 0 1 1 "
  ,"0 1 0 1 0 1 1 0 1 0 0 1 1 1 0 1 0 0 0 "
  ,"1 0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 1 1 "
  ,"1 1 1 0 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 "
  ,"0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 "
  ,"1 1 0 1 0 1 1 1 0 1 0 1 1 0 1 1 0 1 1 "
  ,"1 1 0 1 1 0 1 0 1 1 0 1 1 0 1 1 0 1 1 "
  ]



--runGame :: [[Int]] -> IO ()
runGame = mapM_ putArr . runGame'
runGame' :: [[Int]] -> [([[Int]], Int)]
runGame' = (flip zip [0..]) . (++ [blankScreen]) . takeWhile ((>0) . sumOfBoard) . iterate step . rotate2D 30 15 
  where sumOfBoard = sum . map sum

fps = 25 :: Int

main :: IO ()
main = runGame dieHard 
