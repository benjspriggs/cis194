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
-- streams, Stream a.
-- Write a function that converts from streams
-- to infinite lists:
-- streamToList :: Stream a -> [a]
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : streamToList t

-- Exercise 4
-- Write a function which generates a stream 
-- containing infinitely many copies of the
-- given element.
-- streamRepeat :: a -> Stream a
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- Write a function which applies 
-- a function to every element of a Stream.
-- streamMap :: (a -> b) -> Stream a -> Stream b
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = 
  (Cons (f a) (streamMap f as))

-- Write a function which generates 
-- a Stream from a “seed” of type a,
-- which is the first element of the stream,
-- and an “unfolding rule” of type a -> a
-- which specifies how to transform the seed 
-- into a new seed, to be used for generating 
-- the rest of the stream.
-- streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed rule seed = 
  Cons seed (streamFromSeed rule (rule seed))
