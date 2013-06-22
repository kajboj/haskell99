-- Problem 1

myLast xs = if null(tail xs)
            then
              head xs
            else
              myLast(tail xs)

myLast' :: [a] -> a
myLast' [x] = x
myLast' (_:xs) = myLast xs


-- Problem 2

myButLast xs = myLast(init xs)

myButLast' :: [a] -> a
myButLast' [x, y] = x
myButLast' (_:xs) = myButLast' xs

myButLast'' = head . tail . reverse


-- Problem 3

elementAt (x:_) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

elementAt' xs n = if n == 1
                  then
                    head xs
                  else
                    elementAt (tail xs) (n-1)

-- Problem 4

myLength []     = 0
myLength (x:xs) = 1 + myLength xs


-- Problem 5

myReverse [] = []
myReverse (x:xs) = myReverse(xs) ++ [x]


-- Problem 6

isPalindrome list = list == myReverse list

isPalindrome' [] = True
isPalindrome' list = if head list == myLast list
                     then
                       isPalindrome(tail(init list))
                     else
                       False

-- Problem 7
-- Flatten a nested list structure.

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
-- to be continued

-- Problem 8

compress :: (Eq a) => [a] -> [a]

compress []  = []
compress [x] = [x]
compress (x:xs) = if x == head xs
                  then
                    compress xs
                  else
                    (x : compress xs)

-- Problem 9

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = innerPack xs [x]
  where
    innerPack [] list = [list]
    innerPack (x:xs) list = if x == head list
                            then
                              innerPack xs (x:list)
                            else
                              (list:innerPack xs [x])

-- Problem 10

encode :: (Eq a) => [a] -> [(Int, a)]
encode list = [(myLength s, head s) | s <- pack list]


-- Problem 11

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified x = encodeModified' (encode x)
  where
    encodeModified' [] = []
    encodeModified' ((1, x):xs) = (Single     x : encodeModified' xs)
    encodeModified' ((n, x):xs) = (Multiple n x : encodeModified' xs)


-- Problem 12

decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified (Single x     : xs) = (x : decodeModified xs)
decodeModified (Multiple 2 x : xs) = (x : x : decodeModified xs)
decodeModified (Multiple n x : xs) = (x : decodeModified (Multiple (n-1) x : xs))


-- Problem 13

countElements :: (Eq a, Num n) => [a] -> [(n, a)]
countElements x = inner [] x
  where
    inner e [] = e
    inner [] (r:rs) = inner [(1, r)] rs
    inner ((n, e):es) (r:rs)
      | e == r    = inner ((n+1, e):es) rs
      | otherwise = inner ((1, r):(n, e):es) rs

encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect x = reverse(encodeCounted(countElements x))
  where
    encodeCounted [] = []
    encodeCounted ((1, x):xs) = (Single     x : encodeCounted xs)
    encodeCounted ((n, x):xs) = (Multiple n x : encodeCounted xs)


countElements' :: (Eq a) => [a] -> [(Int, a)]
countElements' = foldr helper []
  where
    helper x [] = [(1, x)]
    helper x ((n, y):ys)
      | x == y     = ((n+1, y):ys)
      | otherwise  = ((1, x):(n, y):ys)

encodeDirect' :: (Eq a) => [a] -> [ListItem a]
encodeDirect' x = map helper (countElements' x)
  where
    helper (1, x) = Single x
    helper (n, x) = Multiple n x


-- Problem 14

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli' :: [a] -> [a]
dupli' = foldr (\x accu -> (x:x:accu)) []


-- Problem 15

repli :: (Num b, Eq b) => [a] -> b -> [a]
repli x n = foldr helper [] x
  where
    helper x accu = rep n x ++ accu

    rep :: (Num b, Eq b) => b -> a -> [a]
    rep 0 y = []
    rep n y = (y : rep (n-1) y)

repli' :: (Num b, Eq b) => [a] -> b -> [a]
repli' x n = helper x n n
  where
    helper [] n m = []
    helper (x:xs) n 0 = helper xs n n
    helper y@(x:xs) n m = (x:helper y n (m-1))

-- Problem 16

dropEvery :: (Num b, Eq b) => [a] -> b -> [a]
dropEvery x n = helper x n n
  where
    helper [] n m = []
    helper (x:xs) n 1 = helper xs n n
    helper (x:xs) n m = (x:helper xs n (m-1))


-- Problem 17

split :: (Eq b, Num b) => [a] -> b -> ([a], [a])
split x n = helper ([], x) n
  where
    helper x 0 = x
    helper (x, (y:ys)) n = helper (x ++ [y], ys) (n-1)


-- Problem 18

slice :: [a] -> Int -> Int -> [a]
slice x n m
  | n <= 0        = slice x 1 m
  | m > length x  = slice x n (length x)
  | m < n         = []
  | otherwise     = snd (split beginning (n-1))
  where
    beginning = fst (split x m)


slice' :: (Eq b, Num b, Ord b) => [a] -> b -> b -> [a]
slice' x n m = discardBefore (discardAfter x m) n
  where
    discardBefore []     n = []
    discardBefore x      1 = x
    discardBefore (x:xs) n = discardBefore xs (n-1)

    discardAfter [] n = []
    discardAfter x  0 = []
    discardAfter (x:xs) n = (x:discardAfter xs (n-1))


-- Problem 19

rotate :: (Eq b, Num b, Ord b) => [a] -> b -> [a]
rotate [] n = []
rotate x 0 = x
rotate x n
  | n > 0 = rotate (tail x ++ [head x]) (n-1)
  | n < 0 = rotate (last x : init x) (n+1)

rotate' :: [a] -> Int -> [a]
rotate' [] n = []
rotate' x n 
  | n == 0 = x
  | n > 0 = join (split x n)
  | n < 0 = join (split x ((length x) + n))

  where
    join x = snd x ++ fst x


-- Problem 20

-- foo x = let
--           a = x
--         in
--           a * 2

removeAt :: (Eq b, Num b) => b -> [a] -> (a, [a])
removeAt n x = let
                 s      = split x (n-1)
                 before = fst s
                 after  = snd s
               in
                 (head after, before ++ tail after)

removeAt' :: Int -> [a] -> (a, [a])
removeAt' n x
  | n < 1        = error "n < 1"
  | n > length x = error "n > length x"
  | otherwise    = helper n [] x
  where
    helper 1 b  (e:es) = (e, b ++ es)
    helper n [] (e:es) = helper (n-1) [e]   es
    helper n b  (e:es) = helper (n-1) (b ++ [e]) es
