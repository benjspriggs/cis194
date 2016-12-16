module Calc where
import ExprT

-- Exercise 1 - Write Version 1 of the Calculator
-- eval :: ExprT -> Integer
-- e.g.:
-- (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
eval :: ExprT -> Integer
eval e = case e of
  Lit n -> n
  Add l r -> eval l + eval r
  Mul l r -> eval l * eval r
