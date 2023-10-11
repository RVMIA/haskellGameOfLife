
module Main where

import Control.Concurrent (threadDelay)



stringProc :: [String] -> [[Int]]
stringProc = map (map read . words)

blankScreen :: [[Int]]
blankScreen = replicate 35 $ replicate 87 0

padToInf :: [[Int]] -> [[Int]]
padToInf = flip (++) (repeat $ repeat 0) . map (++ repeat 0) 

addToBlank :: [[Int]] -> [[Int]]
addToBlank a = sum2D blankScreen $ padToInf a

toUnicode :: Int -> Char
toUnicode 0 = '░'
toUnicode _ = '█'

putArr :: [[Int]] -> IO ()
putArr a = do
  putStrLn $ unlines $ map (Prelude.concatMap (replicate 2 . toUnicode)) a
  threadDelay $ div 1000000 fps 

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

and2D :: [[Int]] -> [[Int]] -> [[Int]]
and2D = zipWith (zipWith and')
  where
    and' :: Int -> Int -> Int
    and' a b
      | a == 0 = 0
      | otherwise = b

step :: [[Int]] -> [[Int]]
step a = sum2D (three b) (and2D (four b) a)
  where
    b = neighbors a
    three = map $ map $ isN 3
    four = map $ map $ isN 4
    isN :: Int -> Int -> Int
    isN n t
      | n == t = 1
      | otherwise = 0

runGame a = mapM_ putArr $ iterate step $ addToBlank a


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

  
fps = 5 :: Int

main :: IO ()
main = runGame glider 
