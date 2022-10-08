module Operation (
    replaceVariable,
    replaceAll,
) where

import Data.Bifunctor (Bifunctor(second))

import LambdaTerm

-- Replaces the variable with the function given.
replaceVariable :: String -> Term -> Term -> Term
replaceVariable v replaceTo (Variable name)
    | name == v = replaceTo
    | otherwise = Variable name
replaceVariable v replaceTo (Abst name cal)
    | name == v = Abst name cal
    | otherwise = Abst name (replaceVariable v replaceTo cal)
replaceVariable v replaceTo (App left right) =
    App (replaceVariable v replaceTo left) (replaceVariable v replaceTo right)

-- Prepends "!" marks to all of the variables in the term.
prependMark :: Term -> Term
prependMark (Variable name) = Variable ("!" ++ name)
prependMark (Abst name child) = Abst name (prependMark child)
prependMark (App child1 child2) = App (prependMark child1) (prependMark child2)

-- Removes "!" marks from the variables.
removeMark :: Term -> Term
removeMark (Variable ('!' : rest)) = Variable rest
removeMark (Variable name) = Variable name
removeMark (Abst name child) = Abst name (removeMark child)
removeMark (App child1 child2) = App (removeMark child1) (removeMark child2)

-- Replaces the variables. Don't confuse with `replaceVariable`.
replaceAll :: [(String, Term)] -> Term -> Term
replaceAll vars cal =
    removeMark
    $ foldl (flip (uncurry replaceVariable)) cal
    $ map (second prependMark) vars
