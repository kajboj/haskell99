data Token = Number (Int) | Op (Operator)
data Operator = Add | Sub | Mul | Div

apply :: Operator -> Int -> Int -> Int
apply Add i j = i + j
apply Sub i j = i - j
apply Mul i j = i * j
apply Div i j = i `div` j

calc :: [Token] -> [Int] -> Int
calc [] (result:[]) = result
calc (Number i:tokens) stack = calc tokens (i:stack)
calc (Op op:tokens) (a:b:stack) = calc tokens (apply op a b:stack)

calc (Op _:tokens) (a:[]) = error "missing arg"
calc (Op _:tokens) []     = error "missing args"

calc [] (result:a:remains) = error "leftover numbers on the stack"