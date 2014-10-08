import Control.Monad.State
import System.Random

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((), (x:xs))

stackManip :: State Stack Int
stackManip = do
  push 3
  pop
  pop

stackManip1 :: State Stack ()
stackManip1 = push 3 >> pop >>= (\x -> push x)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

threeCoins1 :: State StdGen (Bool, Bool, Bool)
threeCoins1 =
  randomSt >>= (\a -> randomSt >>= (\b -> randomSt >>= (\c -> return (a, b, c))))

main :: IO ()
main = do
  putStrLn $ show $ runState stackManip1 [1, 2, 3]
  putStrLn $ show $ runState threeCoins (mkStdGen 33)
  putStrLn $ show $ runState threeCoins1 (mkStdGen 33)


--(>>=) :: m a -> (a -> m b) -> m b
--(>>) :: m a -> m b -> m b
--a >> b = a >>= (\x -> b)
