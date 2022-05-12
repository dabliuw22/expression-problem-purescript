module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Solution
  ( Add(..)
  , Coproduct(..)
  , Expr(..)
  , Div
  , Mul
  , Val(..)
  , eval
  , value
  , (⊕)
  , (⊗)
  , (÷)
  )

main :: Effect Unit
main = do
  let
    addExample :: Expr (Coproduct Val Add)
    addExample =
      In
        ( Inr
            ( Add
                (In (Inl (Val 100)))
                (In (Inl (Val 2)))
            )
        ) -- Add (Val 100) (Val 2)

    addExample2 :: Expr (Coproduct Val Add)
    addExample2 = (value 100) ⊕ (value 2)

    mulExample :: Expr (Coproduct Val (Coproduct Add Mul))
    mulExample = value 80 ⊗ value 5 ⊕ value 4

    divExample :: Expr (Coproduct Val (Coproduct Add (Coproduct Mul Div)))
    divExample = value 80 ⊗ value 5 ⊕ value 4 ÷ value 2

    oterExample :: Expr (Coproduct Val (Coproduct Add (Coproduct Mul Div)))
    oterExample = value 80 ⊗ value 5 ⊕ value 4 ÷ value 2 ÷ value 2 ⊕ value 2000
  log (show $ eval addExample2)
  log (show $ eval addExample)
  log (show $ eval mulExample)
  log (show $ eval divExample)
  log (show $ eval oterExample)
