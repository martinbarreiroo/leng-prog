module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | n == 1    = Just 0
  | otherwise = collatz (next n) >>= Just . (+1)



next :: Integer -> Integer
next n = if even n then n `div` 2 else 3 * n + 1