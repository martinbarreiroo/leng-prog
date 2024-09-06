module Trie  (Trie(..), left, right, find, decode, toList) where

import Bit

data Trie a = Leaf a | Trie a :-: Trie a deriving (Eq, Show)
trie :: Trie Char
trie = (Leaf 'A' :-: Leaf 'B') :-: Leaf 'C'

left :: Trie a -> Trie a
left (l :-: _) = l
left (Leaf _) = error "Left of: Leaf"

right :: Trie a -> Trie a
right (_ :-: r) = r
right (Leaf _) = error "Right of: Leaf"
  
find::Bits -> Trie a -> a
find (F:bs) (l :-: _) = find bs l
find (T:bs) (_ :-: r) = find bs r
find [] (Leaf a) = a
find _ _ = error "Invalid path"

decode :: Bits -> Trie Char -> String
decode bits trie = decodeHelper bits trie
    where
        decodeHelper :: Bits -> Trie Char -> String
        decodeHelper [] (Leaf c) = [c]
        decodeHelper [] _ = []
        decodeHelper (F:bs) (l :-: _) = decodeHelper bs l
        decodeHelper (T:bs) (_ :-: r) = decodeHelper bs r
        decodeHelper bits (Leaf c) = c : decodeHelper bits trie



toList :: Trie a -> [(a, Bits)]
toList (Leaf a) = [(a, [])]
toList (l :-: r) = map (\(a, bs) -> (a, F:bs)) (toList l) ++ map (\(a, bs) -> (a, T:bs)) (toList r)
