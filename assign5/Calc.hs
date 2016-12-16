module Calc where
import ExprT
import Parser (parseExp)
{-# LANGUAGE TypeSynonymInstances #-} -- for Exercise 5

-- Exercise 1 - Write Version 1 of the Calculator
-- eval :: ExprT -> Integer
-- e.g.:
-- (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
eval :: ExprT -> Integer
eval e = case e of
  Lit n -> n
  Add l r -> eval l + eval r
  Mul l r -> eval l * eval r

-- Exercise 2 - Implement evalString
-- Using the parser from Parser.hs
-- evalString :: String -> Maybe Integer
evalString :: String -> Maybe Integer
evalString s = case parseExp Lit Add Mul s of
  Just e -> Just (eval e)
  Nothing -> Nothing

-- Exercise 3 - Create Expr typeclass
-- Create a type class such that:
-- mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
-- == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
class (Show a) => Expr a where
  lit :: Integer -> a
  add, mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Exercise 4 - Extending Functionality
-- Create instances of Expr for:
-- Integer
-- Bool - every literal <= 0 is False, else True
--   addition ||
--   multiplication &&
-- MinMax
--   addition max
--   multiplication min
-- Mod7
--   all values in the range 0..6, all arithmetic is mod7
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>=0)
  add = (&&)
  mul = (||)

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)

instance Expr MinMax where
  lit = MinMax
  add l r = max l r
  mul l r = min l r

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . mod 7
  add (Mod7 x) (Mod7 y) = Mod7 $ mod 7 (x + y)
  mul (Mod7 x) (Mod7 y) = Mod7 $ mod 7 (x * y)

-- Tests for stuff
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- Exercise 5 
-- Implement a stack such that
-- stackVM exp == Right [IVal exp]
