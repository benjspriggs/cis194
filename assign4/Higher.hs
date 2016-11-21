-- Assignment 4
-- written by Benjamin Spriggs

-- Exercise 1: Wholemeal Programming
-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
--   | even x = (x - 2) * fun1 xs
--   | otherwise = fun1 xs
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 n = foldr1 (*) $ takeWhile even n

-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n  
--   | even n = n + fun2 (n `div` 2)
--   | otherwise = fun2 (3 * n + 1)
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n = evens n + odds n
  where evens = (*2)
        odds = (*3)
