import Data.List

data CodeTree = EmptyTree | Leaf (Char) (Int) | Fork (CodeTree) (CodeTree) [Char] (Int) deriving (Show, Eq)

data Bit = Zero | One

instance Ord CodeTree where
  compare x y = compare (weight x) (weight y)

instance Show Bit where
  show Zero = "0"
  show One = "1"

weight :: CodeTree -> Int
weight (Leaf char weight) = weight
weight (Fork left right chars weight) = weight

chars :: CodeTree -> [Char]
chars (Leaf char weight) = [char]
chars (Fork left right chrs weight) = chrs

makeCodeTree :: CodeTree -> CodeTree -> CodeTree
makeCodeTree l r =
  Fork l r ((chars l) ++ (chars r)) ((weight l) + (weight r))

times :: [Char] -> [(Char, Int)]
times xs = decompose (sort xs)
  where
    decompose [] = []
    decompose (x:xs) = inner (x, 1) xs

    inner :: (Char, Int) -> [Char] -> [(Char, Int)]
    inner (e, n) [] = [(e, n)]
    inner (e, n) (x:xs) = if x == e
      then inner (e, n+1) xs
      else ((e, n):(inner (x, 1) xs))

makeOrderedLeafList :: [Char] -> [CodeTree]
makeOrderedLeafList xs = sort [Leaf c i | (c, i) <- times xs]

combine :: [CodeTree] -> [CodeTree]
combine [] = []
combine [t] = [t]
combine (t:u:ts) = combine $ insert fork ts
  where
    fork = makeCodeTree t u

buildEncodingTree :: [Char] -> CodeTree
buildEncodingTree [] = EmptyTree
buildEncodingTree chars = head $ combine $ makeOrderedLeafList chars

encodeChar :: CodeTree -> Char -> [Bit]
encodeChar (Leaf c w) d = []
encodeChar (Fork l r c w) d = if elem d (chars l)
  then Zero : encodeChar l d
  else One : encodeChar r d

encodeString :: CodeTree -> [Char] -> [Bit]
encodeString tree string = foldl1 (++) $ map encode string
  where
    encode = encodeChar $ tree

encode :: [Char] -> ([Bit], CodeTree)
encode string = (encodeString codeTree string, codeTree)
  where
    codeTree = buildEncodingTree string

decodeChar :: CodeTree -> [Bit] -> (Char, [Bit])
decodeChar (Leaf c w) bits = (c, bits)
decodeChar (Fork l r c w) (Zero:bits) = decodeChar l bits
decodeChar (Fork l r c w) (One:bits) = decodeChar l bits

decodeString :: CodeTree -> [Bit] -> [Char]
decodeString tree [] = []
decodeString tree bits = char : decodeString tree bitsAfter
  where
    (char, bitsAfter) = decodeChar tree bits


-- what happens with message "aaa"?
-- handle error when encoding tree does not contain char