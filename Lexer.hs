module Lexer (
    Token (
        Lambda,
        Ident,
        Sep,
        Equal,
        EOF
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
    | EOF
    deriving (Eq, Show)

data LexerResult
    = Valid [Token]
    | LexError String
    deriving (Eq, Show)

concatResult :: Token -> LexerResult -> LexerResult
concatResult f s =
    case (f, s) of
        (token1, Valid token2) -> Valid (token1 : token2)
        (_, LexError msg) -> LexError msg

tokenize :: String -> LexerResult
tokenize "" = Valid [EOF]
tokenize s =
    case spaceRemoved of
        '.' : rest -> concatResult Sep (tokenize rest)
        '=' : rest -> concatResult Equal (tokenize rest)
        '(' : rest -> concatResult LeftParen (tokenize rest)
        ')' : rest -> concatResult RightParen (tokenize rest)
        _ -> case token of
            "" -> LexError token
            "lambda" -> concatResult Lambda (tokenize next)
            token -> concatResult (Ident token) (tokenize next)
    where
        spaceRemoved = dropWhile isSpace s
        token = takeWhile isAlphaNum spaceRemoved
        next = dropWhile isAlphaNum spaceRemoved
