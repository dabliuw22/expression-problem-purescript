module Partial where

import Prelude

data Expr
  = Val Int
  | Add Expr Expr

eval :: Expr -> Int
eval (Val i) = i

eval (Add x y) = eval x + eval y

data Expr2
  = Mult Expr Expr

-- Either approach 
data ExprGroup
  = ExprLeft Expr
  | ExprRight Expr2

-- Problem: If we add new types of Expr, we must add a new data with either aproach.
