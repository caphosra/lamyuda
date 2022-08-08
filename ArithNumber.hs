module ArithNumber (
    ArithNumber(Zero, Succ, Pred),
    evalStep,
    eval,
    toNum,
) where

data ArithNumber =
    Zero
    | Succ ArithNumber
    | Pred ArithNumber
    deriving (Show, Eq)

evalStep :: ArithNumber -> ArithNumber
evalStep num =
    case num of
        Pred Zero -> Zero
        Pred (Succ x) -> x
        Succ num -> Succ (evalStep num)
        Pred x -> Pred (evalStep x)
        x -> x

eval :: ArithNumber -> ArithNumber
eval num =
    if evalStep num == num
        then num
        else eval (evalStep num)

toNum :: ArithNumber -> Int
toNum num =
    case num of
        Zero -> 0
        Succ x -> toNum x + 1
        Pred x -> max (toNum x - 1) 0
