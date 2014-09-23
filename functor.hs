import Control.Applicative

data MyMaybe a = MyJust a | MyNothing deriving Show

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust $ f x


newtype Pair b a = Pair (a, b)

instance Functor (Pair b) where
  --fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap f (Pair (x, y)) = Pair (f x, y)


newtype HPair a = HPair (a, a) deriving Show

instance Functor HPair where
  fmap f (HPair (x, y)) = HPair (f x, f y)

instance Applicative HPair where
  pure x = HPair (x, x)
  (<*>) (HPair (f, g)) (HPair (x, y)) = HPair (f x, g y)