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

-- Converts a lambda calculus into a human-friendly form.
showLambdaCal :: LambdaCal -> String
showLambdaCal (Variable name) = name
showLambdaCal (Abst name cal) = "(λ " ++ name ++ ". " ++ showLambdaCal cal ++ ")"
showLambdaCal (App left right) = "(" ++ showLambdaCal left ++ " " ++ showLambdaCal right ++ ")"
