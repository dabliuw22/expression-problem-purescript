module OtherSolution (eval, view) where

import Prelude

class ExprSym repr where
  val :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr

infixl 4 add as ⊕

instance exprSymInt :: ExprSym Int where
  val i = i
  neg n = -n
  add x y = x + y

instance exprSymString :: ExprSym String where
  val i = show i
  neg n = "(-" <> n <> ")"
  add x y = "(" <> x <> "+" <> y <> ")"

eval :: Int -> Int
eval = identity

view :: String -> String
view = identity

type Repr a
  = ExprSym a => a

addExample :: forall a. Repr a
addExample = val 4 ⊕ neg (val 2)

-- Extension
class ExprMul repr where
  mul :: repr -> repr -> repr

infixl 5 mul as ⊗

instance exprMulInt :: ExprMul Int where
  mul x y = x * y

instance exprMulString :: ExprMul String where
  mul x y = "(" <> x <> "*" <> y <> ")"

type ExtRepr a
  = ExprMul a => Repr a

mulExample :: forall a. ExtRepr a
mulExample = val 1 ⊗ (val 4 ⊕ neg (val 2))
