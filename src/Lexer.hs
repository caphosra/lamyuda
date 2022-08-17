module Lexer (
    Token (
        Lambda,
        Ident,
        Sep,
        Equal,
        LeftParen,
        RightParen
    ),
    tokenize
) where

import Data.Char (isAlphaNum, isSpace)

import Result

-- A token that is used to represent the calculi.
data Token
    = Lambda        -- "lambda" (an alternative form of "λ")
    | Ident String  -- an arbitrary identifier, such as "x"
    | Sep           -- "."
    | Equal         -- "="
    | LeftParen     -- "("
    | RightParen    -- ")"
    deriving (Eq, Show)

-- Holds a result of lexing.
type LexerResult = Result [Token] String

-- Concatenates a token with the result of lexing on a part of the string.
concatResult :: Token -> LexerResult -> LexerResult
concatResult f s =
    case (f, s) of
        (token1, Valid token2) -> Valid (token1 : token2)
        (_, Error msg) -> Error msg

-- Tokenizes characters.
tokenize :: String -> LexerResult
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
            name -> concatResult (Ident name) (tokenize next)
    where
        spaceRemoved = dropWhile isSpace s
        token = takeWhile isAlphaNum spaceRemoved
        next = dropWhile isAlphaNum spaceRemoved
