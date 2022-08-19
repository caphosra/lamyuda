module Operation (
    replaceVariable,
    replaceAll,
    betaNO,
    betaCN,
    betaCV,
) where

import Data.Bifunctor (Bifunctor(second))

import LambdaCalculus

-- Replaces the variable with the function given.
replaceVariable :: String -> LambdaCal -> LambdaCal -> LambdaCal
replaceVariable v replaceTo (Variable name)
    | name == v = replaceTo
    | otherwise = Variable name
replaceVariable v replaceTo (Abst name cal)
    | name == v = Abst name cal
    | otherwise = Abst name (replaceVariable v replaceTo cal)
replaceVariable v replaceTo (App left right) =
    App (replaceVariable v replaceTo left) (replaceVariable v replaceTo right)

-- Prepends "!" marks to all of the variables in the term.
prependMark :: LambdaCal -> LambdaCal
prependMark (Variable name) = Variable ("!" ++ name)
prependMark (Abst name child) = Abst name (prependMark child)
prependMark (App child1 child2) = App (prependMark child1) (prependMark child2)

-- Removes "!" marks from the variables.
removeMark :: LambdaCal -> LambdaCal
removeMark (Variable ('!' : rest)) = Variable rest
removeMark (Variable name) = Variable name
removeMark (Abst name child) = Abst name (removeMark child)
removeMark (App child1 child2) = App (removeMark child1) (removeMark child2)

-- Replaces the variables. Don't confuse with `replaceVariable`.
replaceAll :: [(String, LambdaCal)] -> LambdaCal -> LambdaCal
replaceAll vars cal =
    removeMark
    $ foldl (flip (uncurry replaceVariable)) cal
    $ map (second prependMark) vars

-- Conducts beta-reduction with the normal form strategy.
betaNO :: LambdaCal -> LambdaCal
betaNO (Abst name child) = Abst name (betaNO child)
betaNO (App (Abst name child) replaceTo) = replaceVariable name replaceTo child
betaNO (App left right)
    | betaNO left /= left = App (betaNO left) right
    | otherwise = App left (betaNO right)
betaNO cal = cal

-- Conducts beta-reduction with the call by name strategy.
betaCN :: LambdaCal -> LambdaCal
betaCN (App (Abst name child) replaceTo) = replaceVariable name replaceTo child
betaCN (App left right)
    | betaCN left /= left = App (betaCN left) right
    | otherwise = App left (betaCN right)
betaCN cal = cal

-- Conducts beta-reduction with the call by value strategy.
betaCV :: LambdaCal -> LambdaCal
betaCV (App (Abst name1 child1) (Abst name2 child2)) =
    replaceVariable name1 (Abst name2 child2) child1
betaCV (App left right)
    | betaCV left /= left = App (betaCV left) right
    | otherwise = App left (betaCV right)
betaCV cal = cal
