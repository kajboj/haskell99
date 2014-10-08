import Control.Monad.Writer
   
logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["hello"]
    return (a*b) 

multWithLog1 :: Writer [String] Int  
multWithLog1 = do  
  logNumber 3 >>= (\x -> (logNumber 5 >>= (\y -> return (x*y))))

gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b   
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a  
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)   

main = do
  putStrLn $ show $ runWriter multWithLog


newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList list = DiffList (list++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList ([]++)
  (DiffList f) `mappend` (DiffList g) = DiffList (f . g)