import Prelude
import qualified Data.List

type Stack = [Char]

data Operator = Drop | Nip | Over | Dup | Tuck | Swap | Rot |
  MRot | Drop2 | Nip2 | Dup2 | Over2 | Tuck2 | Swap2 | Rot2 deriving Show

type Sequence = [Operator]

ops = [Drop, Nip, Over, Dup, Tuck, Swap, Rot, MRot, Drop2, Nip2,
  Dup2, Over2, Tuck2, Swap2, Rot2]

maxSeqLength = 5

apply :: Operator -> Stack -> Maybe Stack
apply Drop (x:xs) = Just xs
apply Drop x = Nothing
apply Nip (x1:x2:xs) = Just (x1:xs)
apply Nip x = Nothing
apply Over (x1:x2:xs) = Just (x2:x1:x2:xs)
apply Over x = Nothing
apply Dup (x:xs) = Just (x:x:xs)
apply Dup x = Nothing
apply Tuck (x1:x2:xs) = Just (x1:x2:x1:xs)
apply Tuck x = Nothing
apply Swap (x1:x2:xs) = Just (x2:x1:xs)
apply Swap x = Nothing
apply Rot (x1:x2:x3:xs) = Just (x3:x1:x2:xs)
apply Rot x = Nothing
apply MRot (x1:x2:x3:xs) = Just (x2:x3:x1:xs)
apply MRot x = Nothing
apply Drop2 (x1:x2:xs) = Just xs
apply Drop2 x = Nothing
apply Nip2 (x1:x2:x3:x4:xs) = Just (x1:x2:xs)
apply Nip2 x = Nothing
apply Dup2 (x1:x2:xs) = Just (x1:x2:x1:x2:xs)
apply Dup2 x = Nothing
apply Over2 (x1:x2:x3:x4:xs) = Just (x3:x4:x1:x2:x3:x4:xs)
apply Over2 x = Nothing
apply Tuck2 (x1:x2:x3:x4:xs) = Just (x1:x2:x3:x4:x1:x2:xs)
apply Tuck2 x = Nothing
apply Swap2 (x1:x2:x3:x4:xs) = Just (x3:x4:x1:x2:xs)
apply Swap2 x = Nothing
apply Rot2 (x1:x2:x3:x4:x5:x6:xs) = Just (x5:x6:x1:x2:x3:x4:xs)
apply Rot2 x = Nothing

seqs :: Int -> [Sequence]
seqs 0 = [[]]
seqs n = [(x:y) | x <- ops, y <- seqs $ n-1]

allSeqs :: [[Sequence]]
allSeqs = map seqs [0..maxSeqLength] 

applySeq :: Sequence -> Stack -> Maybe Stack
applySeq seq stack = foldl (>>=) (return stack) (map apply seq)

correctSeqs :: [Sequence] -> Stack -> Stack -> [Sequence]
correctSeqs seqs input output = filter correct seqs
  where
    correct seq = case applySeq seq input of
      Nothing -> False
      Just stack -> stack == output

allAnswers :: [[Sequence]] -> Stack -> Stack -> Maybe [Sequence]
allAnswers allSeqs input output =
  Data.List.find (not . null) (map correct allSeqs)
    where
      correct seqs = correctSeqs seqs input output

main :: IO ()
main = do
  putStrLn . show $ allAnswers allSeqs "abc" "cba"