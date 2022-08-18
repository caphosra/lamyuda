module LambdaCalculus (
    LambdaCal (
        Variable,
        Abst,
        App
    ),
    showLambdaCal,
) where

-- A representation of a lambda calculus.
data LambdaCal
    = Variable String           -- x
    | Abst String LambdaCal     -- λx. M
    | App LambdaCal LambdaCal   -- M M
    deriving (Eq, Show)

-- Wraps a calculus with parentheses if it is not a variable.
showCalWithParen :: LambdaCal -> String
showCalWithParen (Variable name) = name
showCalWithParen cal = "("++ showLambdaCal cal ++ ")"

-- Formats a lambda calculus into human-friendly.
showLambdaCal :: LambdaCal -> String
showLambdaCal (Variable name) = name
showLambdaCal (Abst name cal) = "λ" ++ name ++ ". " ++ showLambdaCal cal
showLambdaCal (App (App cal1 cal2) cal3) = showLambdaCal (App cal1 cal2) ++ " " ++ showCalWithParen cal3
showLambdaCal (App cal1 cal2) = showCalWithParen cal1 ++ " " ++ showCalWithParen cal2
