module Calc where
import ExprT
import Parser (parseExp)

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


