module Lexer (
    Token (
        Lambda,
        Ident,
        Sep,
        Equal,
        LeftParen,
        RightParen,
        Command
    ),
    toStr,
    lengthOfToken,
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
type LexerResult = Result [(Int, Token)] (Int, String)

-- Converts a token to a string.
toStr :: Token -> String
toStr Lambda = "lambda"
toStr (Ident name) = name
toStr Sep = "."
toStr Equal = "="
toStr LeftParen = "("
toStr RightParen = ")"
toStr Command = "#"

-- Returns the length of the token.
lengthOfToken :: Token -> Int
lengthOfToken = length . toStr

-- Prepends a token to the result of lexing.
prependTokens :: Int -> Token -> String -> LexerResult
prependTokens pos token rest =
    mapResult (tokenize' nextPos rest) ((pos, token) :)
    where
        nextPos = pos + lengthOfToken token

-- Tokenizes characters.
tokenize :: String -> LexerResult
tokenize = tokenize' 0

-- Tokenizes characters. It also tracks the position.
tokenize' :: Int -> String -> LexerResult
tokenize' _ "" = Valid []
tokenize' pos (' ' : rest) = tokenize' (pos + 1) rest
tokenize' pos ('.' : rest) = prependTokens pos Sep rest
tokenize' pos ('=' : rest) = prependTokens pos Equal rest
tokenize' pos ('(' : rest) = prependTokens pos LeftParen rest
tokenize' pos (')' : rest) = prependTokens pos RightParen rest
tokenize' pos ('#' : rest) = prependTokens pos Command rest
tokenize' pos s
    | token == "" = Error (pos, take 1 s)
    | token == "lambda" = prependTokens pos Lambda next
    | otherwise = prependTokens pos (Ident token) next
    where
        token = takeWhile isAlphaNum s
        next = dropWhile isAlphaNum s
