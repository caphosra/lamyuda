module Operation (
    substVariable,
    substAll,
) where

import Data.Bifunctor (Bifunctor(second))

import LambdaTerm

--
-- Substitutes the variable with the term.
--
substVariable :: String -> Term -> Term -> Term

substVariable v subst (Variable name)
    | name == v = subst
    | otherwise = Variable name

substVariable v subst (Abst name term)
    | name == v = Abst name term
    | otherwise = Abst name (substVariable v subst term)

substVariable v subst (App left right) =
    App (substVariable v subst left) (substVariable v subst right)

--
-- Prepends "!" marks to all of the variables in the term.
--
prependMark :: Term -> Term

prependMark (Variable name) = Variable ("!" ++ name)

prependMark (Abst name child) = Abst name (prependMark child)

prependMark (App child1 child2) = App (prependMark child1) (prependMark child2)

--
-- Removes "!" marks from the variables.
--
removeMark :: Term -> Term

removeMark (Variable ('!' : rest)) = Variable rest

removeMark (Variable name) = Variable name

removeMark (Abst name child) = Abst name (removeMark child)

removeMark (App child1 child2) = App (removeMark child1) (removeMark child2)

--
-- Substitutes the variables. Don't confuse with `substVariable`.
--
substAll :: [(String, Term)] -> Term -> Term

substAll vars term =
    removeMark
    $ foldl (flip (uncurry substVariable)) term
    $ map (second prependMark) vars
