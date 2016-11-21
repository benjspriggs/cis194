-- Assignment 3 - cis194
-- written by Benjamin Spriggs

module Golf where

import Data.List
-- Exercise 1 - Hopscotch
skips :: [a] -> [[a]]
skips [] = [[]]
skips a =
  map (\x -> every x a) [1..length a]

every :: Int -> [a] -> [a]
every n a = 
  case drop (n - 1) a of
    (a:as) -> a: every n as
    [] -> []

-- Exercise 2 - Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima = map fst . filter (\(x,y) -> x > y) . pairwise

pairwise :: [a] -> [(a,a)]
pairwise as = zip as (tail as)

-- Exercise 3 - Histogram
zeroToNine = [0 .. 9]

histogram :: [Integer] -> String
histogram l =
  unlines (map (stars ls) [max,max-1..1]) ++ "==========\n0123456789\n"
  where ls = frequencies l
        max = maximum ls

frequencies l = map (\x -> count x l) zeroToNine
-- make the stars
stars ls n = map (\x -> if x >= n then '*' else ' ') ls

decrement = map (\x -> if x > 0 then x - 1 else 0)
-- onlyValid = filter $ flip elem zeroToNine

count :: Eq a => a -> [a] -> Integer
count x = genericLength . filter (x==)

-- take all of the valid numbers
-- map them over zeroToNine
-- count the number of times it shows in zeroToNine
countInZeroToNine l = map (\x -> length x) $ group $ sort $ filter (\x -> elem x zeroToNine) l
