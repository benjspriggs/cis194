-- Homework 6 - Lazy Evaluation
-- Benjamin Spriggs

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
