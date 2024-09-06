module Main where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Tuple(swap)
import System.Environment (getArgs)
import Distribution.PackageDescription (mainLibSet)

type Frequency = (Int, Char)

frequencies::String -> [Frequency]
frequencies xs = insertionSort (map swap (Map.toList (frequencyMap xs)))

frequencyMap::(Ord a) => [a] -> Map a Int
frequencyMap (x:xs) = Map.insertWith (+) x 1 (frequencyMap xs)
frequencyMap [] = Map.empty

insert::(Ord a) => a -> [a] -> [a]
insert e [] = [e]
insert e (x:xs)
  | e <= x    = e : x : xs
  | otherwise = x : insert e xs

reverseInsert :: (Ord a) => a -> [a] -> [a]
reverseInsert e [] = [e]
reverseInsert e (x:xs)
  | e >= x    = e : x : xs
  | otherwise = x : reverseInsert e xs

insertionSort::(Ord a) => [a] -> [a]
insertionSort (x:xs) = insert x (insertionSort xs)
insertionSort [] = []

{-
4.Create a `main` function that:
* Takes as an argument a file name (Use `getArgs`)
* If the file name is not specified prints an error message
* Reads the file (Using `readFile`).
* Counts the frequencies of the letters in the file
* Prints the table of frequencies in **REVERSE** order (Starting from the higher ones).

```shell
> freq example.txt | head
' ': 292
'e': 148
'o': 124
't': 98
'h': 96
'n': 95
'a': 93
's': 92
'r': 68
'i': 65
-}

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Error. Usage: freq <filename>"
        (x:_) -> do
            content <- readFile x
            let freqs = reverse (frequencies content)
            mapM_ print freqs
            

            