module Preprocess (
    substBuiltin,
) where

import Text.Read

import LambdaTerm
import Operation

--
-- Replaces a number with its Church encoding.
--
substNum :: Term -> Term
substNum (Variable name) =
    case readMaybe name :: Maybe Int of
        Just num -> generateNum num
        Nothing -> Variable name
  where
    --
    -- Generates a number in Church encoding.
    --
    generateNum :: Int -> Term

    generateNum n =
        Abst "s" (Abst "z" (iterate (App (Variable "s")) (Variable "z") !! n))
substNum (Abst name term) = Abst name (substNum term)
substNum (App left right) = App (substNum left) (substNum right)

--
-- Replaces the variables with the builtin functions.
--
substBuiltin :: Term -> Term
substBuiltin =
    substNum
        . substVariable "true" (Abst "t" (Abst "f" (Variable "t")))
        . substVariable "false" (Abst "t" (Abst "f" (Variable "f")))
        . substVariable "test" (Abst "b" (Abst "c" (Abst "d" (App (App (Variable "b") (Variable "c")) (Variable "d")))))
