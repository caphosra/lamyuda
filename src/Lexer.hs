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

import Data.Char (isAlphaNum)

import Result

-- A token that is used to represent the calculi.
data Token
    = Lambda        -- "lambda" (an alternative form of "λ")
    | Ident String  -- an arbitrary identifier, such as "x"
    | Sep           -- "."
    | Equal         -- "="
    | LeftParen     -- "("
    | RightParen    -- ")"
    | Command       -- "#"
    deriving (Eq, Show)

-- Holds a result of lexing.
type LexerResult = Result [Token] String

-- Tokenizes characters.
tokenize :: String -> LexerResult
tokenize "" = Valid []
tokenize (' ' : rest) = tokenize rest
tokenize ('.' : rest) = mapResult (tokenize rest) (Sep :)
tokenize ('=' : rest) = mapResult (tokenize rest) (Equal :)
tokenize ('(' : rest) = mapResult (tokenize rest) (LeftParen :)
tokenize (')' : rest) = mapResult (tokenize rest) (RightParen :)
tokenize ('#' : rest) = mapResult (tokenize rest) (Command :)
tokenize s
    | token == "" = Error (take 1 token)
    | token == "lambda" = mapResult (tokenize next) (Lambda :)
    | otherwise = mapResult (tokenize next) (Ident token :)
    where
        token = takeWhile isAlphaNum s
        next = dropWhile isAlphaNum s
