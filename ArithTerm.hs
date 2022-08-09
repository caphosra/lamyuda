module ArithTerm (
    ArithTerm (
        ATrue,
        AFalse,
        Zero,
        If,
        Succ,
        Pred,
        IsZero
    ),
    isValue,
    isNum,
    evalStep,
    eval,
) where

-- A term defined by several rules
data ArithTerm
    = ATrue
    | AFalse
    | Zero
    | If ArithTerm ArithTerm ArithTerm
    | Succ ArithTerm
    | Pred ArithTerm
    | IsZero ArithTerm
    deriving (Eq, Show)

isValue :: ArithTerm -> Bool
isValue term =
    case term of
        ATrue -> True
        AFalse -> True
        num | isNum num -> True
        _ -> False

isNum :: ArithTerm -> Bool
isNum term =
    case term of
        Zero -> True
        Succ term -> isNum term
        _ -> False

evalStep :: ArithTerm -> ArithTerm
evalStep term =
    case term of
        -- E-IfTrue
        If ATrue t2 t3 -> t2
        -- E-IfFalse
        If AFalse t2 t3 -> t3
        -- E-If
        If t1 t2 t3 -> If (evalStep t1) t2 t3
        -- E-PredZero
        Pred Zero -> Zero
        -- E-PredSucc
        Pred (Succ t)
            | isNum t -> t
            | otherwise -> term
        -- E-Succ
        Succ t -> Succ (evalStep t)
        -- E-Pred
        Pred t -> Pred (evalStep t)
        -- E-IsZeroZero
        IsZero Zero -> ATrue
        -- E-IsZeroSucc
        IsZero (Succ t)
            | isNum t -> AFalse
            | otherwise -> term
        -- E-IsZero
        IsZero t -> IsZero (evalStep t)
        -- No derivation
        _ -> term

eval :: ArithTerm -> ArithTerm
eval term
    | term == evaluated = term
    | otherwise = eval evaluated
    where
        evaluated = evalStep term
