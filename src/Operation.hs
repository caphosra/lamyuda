module Operation (
    replaceBuiltin,
    betaNO,
    betaCN,
    betaCV,
) where

import Text.Read

import LambdaCalculus

-- Generates a number in Church encoding.
generateNum :: Int -> LambdaCal
generateNum n =
    Abst "s" (Abst "z" (iterate (App (Variable "s")) (Variable "z") !! n))

-- Replaces the variables with the builtin functions.
replaceBuiltin :: LambdaCal -> LambdaCal
replaceBuiltin (Variable name) =
    case readMaybe name :: Maybe Int of
        Just num -> generateNum num
        Nothing ->
            case name of
                "true" -> Abst "t" (Abst "f" (Variable "t"))
                "false" -> Abst "t" (Abst "f" (Variable "f"))
                _ -> Variable name
replaceBuiltin (Abst name cal) = Abst name (replaceBuiltin cal)
replaceBuiltin (App left right) = App (replaceBuiltin left) (replaceBuiltin right)

-- Replaces the variable with the function given.
replaceVariable :: LambdaCal -> String -> LambdaCal -> LambdaCal
replaceVariable (Variable name) v replaceTo
    | name == v = replaceTo
    | otherwise = Variable name
replaceVariable (Abst name cal) v replaceTo
    | name == v = Abst name cal
    | otherwise = Abst name (replaceVariable cal v replaceTo)
replaceVariable (App left right) v replaceTo =
    App (replaceVariable left v replaceTo) (replaceVariable right v replaceTo)

-- Conducts beta-reduction with the normal form strategy.
betaNO :: LambdaCal -> LambdaCal
betaNO (Abst name child) = Abst name (betaNO child)
betaNO (App (Abst name child) replaceTo) = replaceVariable child name replaceTo
betaNO (App left right)
    | betaNO left /= left = App (betaNO left) right
    | otherwise = App left (betaNO right)
betaNO cal = cal

-- Conducts beta-reduction with the call by name strategy.
betaCN :: LambdaCal -> LambdaCal
betaCN (App (Abst name child) replaceTo) = replaceVariable child name replaceTo
betaCN (App left right)
    | betaCN left /= left = App (betaCN left) right
    | otherwise = App left (betaCN right)
betaCN cal = cal

-- Conducts beta-reduction with the call by value strategy.
betaCV :: LambdaCal -> LambdaCal
betaCV (App (Abst name1 child1) (Abst name2 child2)) =
    replaceVariable child1 name1 (Abst name2 child2)
betaCV (App left right)
    | betaCV left /= left = App (betaCV left) right
    | otherwise = App left (betaCV right)
betaCV cal = cal
