import qualified Data.List

data Operator = Drop | Nip | Over | Dup | Tuck | Swap | Rot |
  MRot | Drop2 | Nip2 | Dup2 | Over2 | Tuck2 | Swap2 | Rot2 deriving Show

ops = [Drop, Nip, Over, Dup, Tuck, Swap, Rot, MRot, Drop2, Nip2,
  Dup2, Over2, Tuck2, Swap2, Rot2]
maxSeqLength = 5 

isApplicable' :: Operator -> Int -> Bool
isApplicable' op length = case op of 
  Drop -> length > 0
  Dup  -> length > 0
  Nip  -> length > 1
  Over -> length > 1
  Tuck -> length > 1
  Swap -> length > 1
  Rot  -> length > 2
  MRot -> length > 2
  Drop2 -> length > 1
  Nip2 -> length > 3
  Dup2 -> length > 1
  Over2 -> length > 3
  Tuck2 -> length > 3
  Swap2 -> length > 3
  Rot2 -> length > 5

apply :: Operator -> [Char] -> [Char]
apply Drop (x:xs) = xs
apply Nip (x1:x2:xs) = (x1:xs)
apply Over (x1:x2:xs) = (x2:x1:x2:xs)
apply Dup (x:xs) = (x:x:xs)
apply Tuck (x1:x2:xs) = (x1:x2:x1:xs)
apply Swap (x1:x2:xs) = (x2:x1:xs)
apply Rot (x1:x2:x3:xs) = (x3:x1:x2:xs)
apply MRot (x1:x2:x3:xs) = (x2:x3:x1:xs)
apply Drop2 (x1:x2:xs) = xs
apply Nip2 (x1:x2:x3:x4:xs) = (x1:x2:xs)
apply Dup2 (x1:x2:xs) = (x1:x2:x1:x2:xs)
apply Over2 (x1:x2:x3:x4:xs) = (x3:x4:x1:x2:x3:x4:xs)
apply Tuck2 (x1:x2:x3:x4:xs) = (x1:x2:x3:x4:x1:x2:xs)
apply Swap2 (x1:x2:x3:x4:xs) = (x3:x4:x1:x2:xs)
apply Rot2 (x1:x2:x3:x4:x5:x6:xs) = (x5:x6:x1:x2:x3:x4:xs)

isApplicable :: Operator -> [Char] -> Bool
isApplicable op stack = isApplicable' op (length stack)

seqs :: Int -> [[Operator]]
seqs 0 = [[]]
seqs n = [(x:y) | x <- ops, y <- seqs $ n-1]

applySeq :: [Operator] -> [Char] -> [Char]
applySeq ops stack = foldl (flip apply) stack ops

isSeqApplicable :: [Operator] -> [Char] -> Bool
isSeqApplicable [] stack = True
isSeqApplicable (op:ops) stack =
  (isApplicable op stack) && (isSeqApplicable ops (apply op stack))

allSeqs :: [[[Operator]]]
allSeqs = map seqs [0..maxSeqLength] 

allAnswers :: [[[Operator]]] -> [Char] -> [Char] -> Maybe [[Operator]]
allAnswers allSeqs input output =
  Data.List.find (not . null) (map satisfying allSeqs)
    where
      satisfying seqs = filter (\seq -> (applicable seq) && (correct seq)) seqs
        where
          applicable seq = isSeqApplicable seq input
          correct seq = applySeq seq input == output

