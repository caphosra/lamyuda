module Operation (
    replaceVariable,
    betaNO,
    betaCN,
    betaCV,
) where

import LambdaCalculus

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
