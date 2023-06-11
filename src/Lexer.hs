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
    tokenize,
) where

import Data.Char (isAlphaNum)

import Result

--
-- A token that is used to represent the term.
--
data Token
    = Lambda -- "lambda" (an alternative form of "λ")
    | Ident String -- an arbitrary identifier, such as "x"
    | Sep -- "."
    | Equal -- "="
    | LeftParen -- "("
    | RightParen -- ")"
    | Command -- "#"
    | Text String -- a text, such as "\"some\""
    deriving (Eq)

instance Show Token where
    show Lambda = "lambda"
    show (Ident name) = name
    show Sep = "."
    show Equal = "="
    show LeftParen = "("
    show RightParen = ")"
    show Command = "#"
    show (Text name) = "\"" ++ name ++ "\""

--
-- Holds a result of lexing.
--
type LexerResult = Result [(Int, Token)] (Int, String)

--
-- Tokenizes characters.
--
tokenize :: String -> LexerResult
tokenize = tokenize' 0
  where
    --
    -- Prepends a token to the result of lexing.
    --
    prependTokens :: Int -> Token -> String -> LexerResult

    prependTokens pos token rest =
        mapResult (tokenize' nextPos rest) ((pos, token) :)
      where
        nextPos = pos + length (show token)

    --
    -- Tokenizes characters. It also tracks the position.
    --
    tokenize' :: Int -> String -> LexerResult

    tokenize' _ "" = Valid []
    tokenize' pos (' ' : rest) = tokenize' (pos + 1) rest
    tokenize' pos ('.' : rest) = prependTokens pos Sep rest
    tokenize' pos ('=' : rest) = prependTokens pos Equal rest
    tokenize' pos ('(' : rest) = prependTokens pos LeftParen rest
    tokenize' pos (')' : rest) = prependTokens pos RightParen rest
    tokenize' pos ('#' : rest) = prependTokens pos Command rest
    tokenize' pos ('"' : rest) =
        prependTokens pos (Text text) remain
      where
        text = takeWhile (/= '"') rest
        remain = drop 1 $ dropWhile (/= '"') rest
    tokenize' pos ('\\' : rest) =
        prependTokens pos Lambda rest
    tokenize' pos s
        | token == "" = Error (pos, take 1 s)
        | token == "lambda" = prependTokens pos Lambda next
        | token == "lmd" = prependTokens pos Lambda next
        | otherwise = prependTokens pos (Ident token) next
      where
        token = takeWhile isAlphaNum s
        next = dropWhile isAlphaNum s
