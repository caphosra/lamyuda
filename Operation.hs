module Operation (
    showLambdaCal,
    replaceBuiltin,
    betaNO,
    betaCN,
    betaCV,
) where

import Text.Read
import Parser

showLambdaCal :: LambdaCal -> String
showLambdaCal (Variable name) = name
showLambdaCal (Abst name cal) = "(λ " ++ name ++ ". " ++ showLambdaCal cal ++ ")"
showLambdaCal (App left right) = "(" ++ showLambdaCal left ++ " " ++ showLambdaCal right ++ ")"

generateNum :: Int -> LambdaCal
generateNum n =
    Abst "s" (Abst "z" (iterate (App (Variable "s")) (Variable "z") !! n))

replaceBuiltin :: LambdaCal -> LambdaCal
replaceBuiltin (Variable name) =
    case num of
        Just num -> generateNum num
        Nothing ->
            case name of
                "true" -> Abst "t" (Abst "f" (Variable "t"))
                "false" -> Abst "t" (Abst "f" (Variable "f"))
                _ -> Variable name
    where
        num = readMaybe name :: Maybe Int
replaceBuiltin (Abst name cal) = Abst name (replaceBuiltin cal)
replaceBuiltin (App left right) = App (replaceBuiltin left) (replaceBuiltin right)

replaceVariable :: LambdaCal -> String -> LambdaCal -> LambdaCal
replaceVariable (Variable name) v replaceTo =
    if name == v then
        replaceTo
    else
        Variable name
replaceVariable (Abst name cal) v replaceTo =
    if name == v then
        Abst name cal
    else
        Abst name (replaceVariable cal v replaceTo)
replaceVariable (App left right) v replaceTo =
    App (replaceVariable left v replaceTo) (replaceVariable right v replaceTo)

betaNO :: LambdaCal -> LambdaCal
betaNO cal =
    case cal of
        Abst name cal -> Abst name (betaNO cal)
        App (Abst name cal) replaceTo -> replaceVariable cal name replaceTo
        App left right ->
            if try /= cal then
                try
            else
                App left (betaNO right)
            where
                try = App (betaNO left) right
        _ -> cal

betaCN :: LambdaCal -> LambdaCal
betaCN cal =
    case cal of
        App (Abst name cal) replaceTo -> replaceVariable cal name replaceTo
        App left right ->
            if try /= cal then
                try
            else
                App left (betaNO right)
            where
                try = App (betaNO left) right
        _ -> cal

betaCV :: LambdaCal -> LambdaCal
betaCV cal =
    case cal of
        App (Abst name1 cal1) (Abst name2 cal2) ->
            replaceVariable cal1 name1 (Abst name2 cal2)
        App left right ->
            if try /= cal then
                try
            else
                App left (betaCV right)
            where
                try = App (betaCV left) right
        _ -> cal
