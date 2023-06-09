module Operation (
    substVariable,
    isFreeVariable,
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
    | isFreeVariable name subst = substVariable v subst $ Abst newName (substVariable name (Variable newName) term)
    | otherwise = Abst name (substVariable v subst term)
    where
        generateNewName name' = if isFreeVariable newName' term
            then generateNewName newName'
            else newName'
            where
                newName' = name' ++ "\'"
        newName = generateNewName name
substVariable v subst (App left right) =
    App (substVariable v subst left) (substVariable v subst right)

--
-- Check whether the variable is captured or not.
--
isFreeVariable :: String -> Term -> Bool

isFreeVariable v (Variable name) = v == name

isFreeVariable v (Abst name term)
    | name == v = False
    | otherwise = isFreeVariable v term

isFreeVariable v (App left right) = isFreeVariable v left || isFreeVariable v right

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

removeMark (Abst name child) = Abst name (removeMark child)

removeMark (App child1 child2) = App (removeMark child1) (removeMark child2)

removeMark term = term

--
-- Substitutes the variables. Don't confuse with `substVariable`.
--
substAll :: Context -> Term -> Term

substAll vars term =
    removeMark
    $ foldl (flip (uncurry substVariable)) term
    $ map (second prependMark) vars
