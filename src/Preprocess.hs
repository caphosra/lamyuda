module Preprocess (
    replaceBuiltin,
) where

import Text.Read

import LambdaCalculus
import Operation

-- Generates a number in Church encoding.
generateNum :: Int -> LambdaCal
generateNum n =
    Abst "s" (Abst "z" (iterate (App (Variable "s")) (Variable "z") !! n))

-- Replaces a number with its Church encoding.
replaceNum :: LambdaCal -> LambdaCal
replaceNum (Variable name) =
    case readMaybe name :: Maybe Int of
        Just num -> generateNum num
        Nothing  -> Variable name
replaceNum (Abst name cal) = Abst name (replaceNum cal)
replaceNum (App left right) = App (replaceNum left) (replaceNum right)

-- Replaces the variables with the builtin functions.
replaceBuiltin :: LambdaCal -> LambdaCal
replaceBuiltin =
    replaceNum
    . (\c -> replaceVariable c "true" (Abst "t" (Abst "f" (Variable "t"))))
    . (\c -> replaceVariable c "false" (Abst "t" (Abst "f" (Variable "f"))))
    . (\c -> replaceVariable c "test" (Abst "b" (Abst "c" (Abst "d" (App (App (Variable "b") (Variable "c")) (Variable "d"))))))
