-- Homework 6 - Lazy Evaluation
-- Benjamin Spriggs

import Data.List -- for genericTake

-- Exercise 1
-- Create a function that computes the
-- nth digit of the Fibonacci sequence
-- fib :: Integer -> Integer

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

-- Define the infinite list of all
-- Fibonacci numbers naively
fibsl :: [Integer]
fibsl = [ fib n | n <- [0..] ]

-- Exercise 2
-- Compute the infinite list of all
-- Fibonacci numbers in linear time
fibs2 :: [Integer]
fibs2 = 0 : 1 : 
  map (\n -> 
    (genericIndex fibs2 (n-1))
  + (genericIndex fibs2 (n-2))) 
  [2..]

-- Exercise 3
-- Define a data type of polymorphic
-- streams, Stream a
-- Write a function that converts from streams
-- to infinite lists:
-- streamToList :: Stream a -> [a]
data Stream a

streamToList :: Stream a -> [a]
streamToList = undefined
