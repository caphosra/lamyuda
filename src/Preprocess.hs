module Preprocess (
    replaceBuiltin,
) where

import Text.Read

import LambdaTerm
import Operation

-- Generates a number in Church encoding.
generateNum :: Int -> Term
generateNum n =
    Abst "s" (Abst "z" (iterate (App (Variable "s")) (Variable "z") !! n))

-- Replaces a number with its Church encoding.
replaceNum :: Term -> Term
replaceNum (Variable name) =
    case readMaybe name :: Maybe Int of
        Just num -> generateNum num
        Nothing  -> Variable name
replaceNum (Abst name cal) = Abst name (replaceNum cal)
replaceNum (App left right) = App (replaceNum left) (replaceNum right)

-- Replaces the variables with the builtin functions.
replaceBuiltin :: Term -> Term
replaceBuiltin =
    replaceNum
    . replaceVariable "true" (Abst "t" (Abst "f" (Variable "t")))
    . replaceVariable "false" (Abst "t" (Abst "f" (Variable "f")))
    . replaceVariable "test" (Abst "b" (Abst "c" (Abst "d" (App (App (Variable "b") (Variable "c")) (Variable "d")))))
