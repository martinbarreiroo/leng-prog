module Lists (member, union, intersection, difference,
              insert, insertionSort, firsts,
              binaryToDecimal, toDecimal, toDec, decimal,
              binaryAdd) where

import Data.Char(digitToInt)

member:: Int -> [Int] -> Bool
member _ []      = False
member e (x:xs)  = e == x || member e xs


union:: [Int] -> [Int] -> [Int]
union [] ys     = ys
union (x:xs) ys
  | member x ys = union xs ys
  | otherwise   = x : union xs ys

intersection:: [Int] -> [Int] -> [Int]
intersection [] _ = []
intersection (x:xs) ys
  | member x ys = x : intersection xs ys -- if x is in ys, then x is in the intersection, so we add it to the result
  | otherwise   = intersection xs ys

difference:: [Int] -> [Int] -> [Int]
difference xs [] = xs -- non-empty minus nullSet
difference [] _  = [] -- nullSet minus non-empty
difference (x:xs) ys
  | member x ys = difference xs ys
  | otherwise   = x : difference xs ys



insert:: Int -> [Int] -> [Int]
insert e [] = [e]
insert e (x:xs)
  | e <= x    = e : x : xs
  | otherwise = x : insert e xs

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)



binaryToDecimal :: [Int] -> Int
binaryToDecimal [] = 0
binaryToDecimal (x:xs) = x * 2 ^ length xs + binaryToDecimal xs


toDecimal :: Int -> [Int] -> Int
toDecimal n list
  | n == 0 = 0
  | null list = 0
  | otherwise = x * n ^ length xs + toDecimal n xs
  where x = head list
        xs = tail list


toDec::Int -> String -> Int
toDec base s = toDecimal base (map digitToInt s)


-- Same as `toDec` But use a list comprehension

decimal::Int -> String -> Int
decimal base s = sum [x * base ^ i | (x, i) <- zip (map digitToInt s) [length s - 1, length s - 2 .. 0]]

firsts::[a] -> [[a]]
firsts [] = []
firsts (x:xs) = [x] : map (x:) (firsts xs)

-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation

binaryAdd::String -> String -> String
binaryAdd a b = decimalToBinary (mySum (toDec 2 a) (toDec 2 b))


decimalToBinary::Int -> String
decimalToBinary 0 = "0"
decimalToBinary n = reverse (decimalToBinary' n)
  where decimalToBinary' 0 = ""
        decimalToBinary' n = show (n `mod` 2) ++ decimalToBinary' (n `div` 2)

mySum::Int -> Int -> Int
mySum a b = a + b
