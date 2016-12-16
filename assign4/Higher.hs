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

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a) deriving (Show)
-- foldTree :: [a] -> Tree a
-- generates a balanced tree from a list
-- using foldr
foldTree :: [a] -> Tree a
foldTree = foldr insertIntoTree Leaf

-- wrong numbers but the other way is
-- dumb
insertIntoTree :: a -> Tree a -> Tree a
insertIntoTree a Leaf = Node 0 Leaf a Leaf
insertIntoTree a (Node h l as r) = 
  Node h left as right
  where (left, right) = case (l, r) of 
          (Leaf, Leaf) -> (newnode, Leaf)
          (Leaf, r) -> (newnode, r)
          (l, Leaf) -> (l, newnode)
          (_ , _) -> if height r > height l 
            then (insertIntoTree a l, r) 
            else (l, insertIntoTree a r)
        newnode = Node (h+1) Leaf a Leaf
        height t = case t of
                    Leaf -> 0
                    Node h l _ r -> 1 + max (height l) (height r)

-- Exercise 3: More folds!

-- xor :: [Bool] -> Bool
-- Return if there an odd number of true values
xor :: [Bool] -> Bool
xor = odd . length . filter id

-- map' :: (a -> b) -> [a] -> [b]
-- Implement map as a foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x l -> (f x):l) []

-- Exercise 4: Finding primes

-- sieveSundaram :: Integer -> [Integer]
-- Generate all prime numbers up to 2n + 2
-- Using the sieve of Sundaram,
-- implemented with function composition
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 
  map (+1) . map (*2) .  filter notInPrimes $ [1..n]
  where primes = [i + j + (2 * i * j) | i <- [1..n], j <- [i..n]]
        notInPrimes = flip notElem primes
