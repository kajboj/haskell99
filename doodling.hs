mySequence :: Monad m => [m a] -> m [a]
mySequence [] = return []
mySequence (m:ms) =
  m >>= (\a -> sequence ms >>= (\as -> return (a:as)))

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx =
  mf >>= (\f -> mx >>= (\x -> return $ f x))

mySequence1 :: Monad m => [m a] -> m [a]
mySequence1 [] = return []
mySequence1 (m:ms) = return (:) `ap` m `ap` sequence ms

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)

a = [ [1, 2, 9]
    , [3, 4, 5] ]

myRepeat :: a -> [a]
myRepeat x = x : repeat x

zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _      _      = []

transpose2 :: [[a]] -> [[a]]
transpose2 [] = repeat []
transpose2 (xs:xss) = repeat (:) `zapp` xs `zapp` transpose2 xss

data Exp v = Var v
           | Val Int
           | Add (Exp v) (Exp v)

type Env v = [(v, Int)]

fetch :: String -> Env String -> Int
fetch s ((k, v):kvs) = if s == k then v else fetch s kvs

eval :: Exp String -> Env String -> Int
eval (Var x)   env = fetch x env
eval (Val i)   env = i
eval (Add p q) env = eval p env + eval q env

k :: a -> env -> a
k x env = x

s :: (env -> a -> b) -> (env -> a) -> (env -> b)
s ef es env = ef env (es env)

eval2 :: Exp String -> Env String -> Int
eval2 (Var x)   = fetch x
eval2 (Val i)   = k i
eval2 (Add p q) = k (+) `s` eval2 p `s` eval2 q

class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
  pure x = Just x
  Just f <*> Just x = Just $ f x
  _      <*> _      = Nothing

class Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a     -> f (t b)
  dist     :: Applicative f =>               t (f a) -> f (t a)
  dist     = traverse id

newtype Id a = An { an :: a } deriving Show

instance Applicative Id where
  pure = An
  An f <*> An x = An (f x)

myFmap f = an . traverse (An . f)
