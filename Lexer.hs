module Lexer (
    Token (
        Lambda,
        Ident,
        Sep,
        Equal,
        LeftParen,
        RightParen
    ),
    Result (
        Valid,
        Error
    ),
    tokenize
) where

import Data.Char (isAlphaNum, isSpace)

data Token
    = Lambda
    | Ident String
    | Sep
    | Equal
    | LeftParen
    | RightParen
    deriving (Eq, Show)

data Result item err
    = Valid item
    | Error err
    deriving (Eq, Show)

type LexResult = Result [Token] String

concatResult :: Token -> LexResult -> LexResult
concatResult f s =
    case (f, s) of
        (token1, Valid token2) -> Valid (token1 : token2)
        (_, Error msg) -> Error msg

tokenize :: String -> LexResult
tokenize s =
    case spaceRemoved of
        "" -> Valid []
        '.' : rest -> concatResult Sep (tokenize rest)
        '=' : rest -> concatResult Equal (tokenize rest)
        '(' : rest -> concatResult LeftParen (tokenize rest)
        ')' : rest -> concatResult RightParen (tokenize rest)
        _ -> case token of
            "" -> Error (take 1 spaceRemoved)
            "lambda" -> concatResult Lambda (tokenize next)
            token -> concatResult (Ident token) (tokenize next)
    where
        spaceRemoved = dropWhile isSpace s
        token = takeWhile isAlphaNum spaceRemoved
        next = dropWhile isAlphaNum spaceRemoved
