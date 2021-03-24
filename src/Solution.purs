module Solution
  ( Add(..)
  , Expr(..)
  , Coproduct(..)
  , Div(..)
  , Mul(..)
  , Val(..)
  , class Injector
  , inj
  , class Eval
  , evaluate
  , eval
  , add
  , div
  , mul
  , value
  , (⊕)
  , (⊗)
  , (÷)
  ) where

import Prelude
  ( class Functor
  , map
  , identity
  , (<<<)
  , (<$>)
  , (+)
  , (*)
  , (/)
  )

-- Define a special "In" type that hides the next
-- `Expr` type from the actual data type
-- `f` is a higher-kinded type
data Expr f
  = In (f (Expr f))

-- newtype Coproduct f g e = Coproduct (Either (f e) (g e))
data Coproduct f g e
  = Inl (f e)
  | Inr (g e)

newtype Val a
  = Val Int

data Add v
  = Add v v

-- derive instance valFunctor :: Functor Val
-- No-op Fucntor
instance valFunctor :: Functor Val where
  map _ (Val x) = Val x

-- derive instance addFunctor :: Functor Add
instance addFunctor :: Functor Add where
  map f (Add e1 e2) = Add (f e1) (f e2)

instance coproductFunctor :: (Functor f, Functor g) => Functor (Coproduct f g) where
  map f (Inl e1) = Inl (map f e1)
  map f (Inr e2) = Inr (map f e2)

class
  (Functor f) <= Eval f where
  evaluate :: f Int -> Int

instance valueEval :: Eval Val where
  evaluate (Val x) = x

instance addEval :: Eval Add where
  evaluate :: Add Int -> Int
  evaluate (Add x y) = x + y

instance coproductEval :: (Eval f, Eval g) => Eval (Coproduct f g) where
  evaluate (Inl f) = evaluate f
  evaluate (Inr g) = evaluate g

fold :: forall a f. (Functor f) => (f a -> a) -> Expr f -> a
fold f (In t) = f ((fold f) <$> t)

eval :: forall f. (Eval f) => Expr f -> Int
eval expr = fold evaluate expr

-- Smart constructors
class
  (Functor fa, Functor fb) <= Injector fa fb where
  inj :: forall a. fa a -> fb a

instance fInj :: Functor f => Injector f f where
  inj :: forall a. f a -> f a
  inj = identity
else instance fgInj :: (Functor f, Functor g) => Injector f (Coproduct f g) where
  inj :: forall a. f a -> Coproduct f g a
  inj = Inl
else instance fghInj :: (Functor f, Functor g, Functor h, Injector f g) => Injector f (Coproduct h g) where
  inj :: forall a. f a -> Coproduct h g a
  inj = Inr <<< inj

inject :: forall g f. (Injector g f) => g (Expr f) -> Expr f
inject = In <<< inj

value :: forall f. (Injector Val f) => Int -> Expr f
value i = inject (Val i)

add :: forall f. (Injector Add f) => Expr f -> Expr f -> Expr f
add x y = inject (Add x y)

infixl 4 add as ⊕

-- Extension
data Mul v
  = Mul v v

instance mulFunctor :: Functor Mul where
  map f (Mul x y) = Mul (f x) (f y)

instance mulEval :: Eval Mul where
  evaluate :: Mul Int -> Int
  evaluate (Mul x y) = x * y

mul :: forall f. (Injector Mul f) => Expr f -> Expr f -> Expr f
mul x y = inject (Mul x y)

infixl 5 mul as ⊗

data Div v
  = Div v v

instance divFunctor :: Functor Div where
  map f (Div x y) = Div (f x) (f y)

instance divEval :: Eval Div where
  evaluate :: Div Int -> Int
  evaluate (Div x y) = x / y

div :: forall f. (Injector Div f) => Expr f -> Expr f -> Expr f
div x y = inject (Div x y)

infixl 5 div as ÷
