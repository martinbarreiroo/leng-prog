module Fraction (Fraction, add, sub, mul, divide, hcf, simplify) where

type Fraction = (Int, Int)

-- Implement the `add` Function
add :: Fraction -> Fraction -> Fraction
add f1 f2 = simplify (n1*d2 + n2*d1, d1*d2)
    where
        (n1, d1) = f1
        (n2, d2) = f2

-- Implement the `sub` Function
sub :: Fraction -> Fraction -> Fraction
sub f1 f2 = simplify (n1*d2 - n2*d1, d1*d2)
    where
        (n1, d1) = f1
        (n2, d2) = f2

-- Implement the `mul` Function
mul :: Fraction -> Fraction -> Fraction
mul f1 f2 = simplify (n1*n2, d1*d2)
    where
        (n1, d1) = f1
        (n2, d2) = f2

-- Implement the `divide` Function
divide :: Fraction -> Fraction -> Fraction
divide f1 f2 = simplify (n1*d2, d1*n2)
    where
        (n1, d1) = f1
        (n2, d2) = f2

-- Implement the `hcf` Function
hcf :: Int -> Int -> Int
hcf n 0 = n
hcf n d = hcf d (n `mod` d)

-- Implement the `simplify` Function
simplify :: Fraction -> Fraction
simplify (n, d) = (n `div` common, d `div` common)
    where
        common = hcf n d


    