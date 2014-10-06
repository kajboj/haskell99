import Prelude
import qualified Data.List
import Control.Monad

type Stack = [Char]

data Operator = Drop | Nip | Over | Dup | Tuck | Swap | Rot |
  MRot | Drop2 | Nip2 | Dup2 | Over2 | Tuck2 | Swap2 | Rot2 deriving Show

type Sequence = [Operator]

ops = [Drop, Nip, Over, Dup, Tuck, Swap, Rot, MRot, Drop2, Nip2,
  Dup2, Over2, Tuck2, Swap2, Rot2]

maxSeqLength = 5

apply :: Operator -> Stack -> Maybe Stack
apply Drop (x:xs) = Just xs
apply Nip (x1:x2:xs) = Just (x1:xs)
apply Over (x1:x2:xs) = Just (x2:x1:x2:xs)
apply Dup (x:xs) = Just (x:x:xs)
apply Tuck (x1:x2:xs) = Just (x1:x2:x1:xs)
apply Swap (x1:x2:xs) = Just (x2:x1:xs)
apply Rot (x1:x2:x3:xs) = Just (x3:x1:x2:xs)
apply MRot (x1:x2:x3:xs) = Just (x2:x3:x1:xs)
apply Drop2 (x1:x2:xs) = Just xs
apply Nip2 (x1:x2:x3:x4:xs) = Just (x1:x2:xs)
apply Dup2 (x1:x2:xs) = Just (x1:x2:x1:x2:xs)
apply Over2 (x1:x2:x3:x4:xs) = Just (x3:x4:x1:x2:x3:x4:xs)
apply Tuck2 (x1:x2:x3:x4:xs) = Just (x1:x2:x3:x4:x1:x2:xs)
apply Swap2 (x1:x2:x3:x4:xs) = Just (x3:x4:x1:x2:xs)
apply Rot2 (x1:x2:x3:x4:x5:x6:xs) = Just (x5:x6:x1:x2:x3:x4:xs)
apply _ _ = Nothing

seqs :: Int -> [Sequence]
seqs n = sequence $ replicate n ops

allSeqs :: [[Sequence]]
allSeqs = map seqs [0..maxSeqLength] 

applySeq :: Stack -> Sequence -> Maybe Stack
applySeq = foldM $ flip apply

correctSeqs :: [Sequence] -> Stack -> Stack -> [Sequence]
correctSeqs seqs input output = filter correct seqs
  where
    correct seq = case applySeq input seq of
      Nothing -> False
      Just stack -> stack == output

allAnswers :: [[Sequence]] -> Stack -> Stack -> Maybe [Sequence]
allAnswers allSeqs input output =
  Data.List.find (not . null) (map correct allSeqs)
    where
      correct seqs = correctSeqs seqs input output

main :: IO ()
main = do
  putStrLn . show $ allAnswers allSeqs "abc" "abcc"