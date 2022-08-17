module Parser (
    Statement (
        FuncDef,
        Eval
    ),
    parseLambdaCal,
    parseStatement
) where

import Data.List (elemIndex)

import LambdaCalculus
import Lexer
import Result

-- A representation of a lambda calculus with parentheses.
data LambdaCal'
    = Variable' String          -- x
    | Abst' String LambdaCal'   -- λx. M
    | App' LambdaCal' LambdaCal'-- M M
    | Paren' LambdaCal'         -- (M)
    deriving (Eq, Show)

-- A representation of a statement.
data Statement
    = FuncDef String LambdaCal
    | Eval LambdaCal
    deriving (Eq, Show)

-- Holds a result of parsing a lambda calculus.
type CalParserResult = Result LambdaCal [Token]

-- Holds a result of parsing a lambda calculus with parentheses.
type CalParserResult' = Result LambdaCal' [Token]

-- Holds a result of parsing.
type ParserResult = Result Statement [Token]

-- Adds +1 if the token is "(" and -1 if ")".
addParenNum :: Int -> Token -> Int
addParenNum n LeftParen = 1 + n
addParenNum n RightParen = -1 + n
addParenNum n _ = n

-- Looks for the corresponding right parenthesis.
findRightParen :: [Token] -> Maybe Int
findRightParen s = elemIndex 0 (scanl addParenNum 1 s)

-- Parses a chain of an application, that is "M M M...".
connectApp :: LambdaCal' -> [Token] -> CalParserResult'
connectApp new [] = Valid new
connectApp new rest =
    case parseLambdaCal' rest of
        Valid (App' left right) -> Valid (App' (App' new left) right)
        Valid cal -> Valid (App' new cal)
        err -> err

-- Removes parentheses from a lambda calculus.
removeParen :: LambdaCal' -> LambdaCal
removeParen (Variable' name) = Variable name
removeParen (Abst' name cal) = Abst name (removeParen cal)
removeParen (App' left right) = App (removeParen left) (removeParen right)
removeParen (Paren' cal) = removeParen cal

-- Parses a lambda calculus.
parseLambdaCal :: [Token] -> CalParserResult
parseLambdaCal s =
    case parseLambdaCal' s of
        Valid cal -> Valid (removeParen cal)
        Error err -> Error err

-- Parses a lambda calculus with parentheses.
parseLambdaCal' :: [Token] -> CalParserResult'
parseLambdaCal' (Lambda : (Ident val) : Sep : rest) =
    case restTerm of
        Valid cal -> Valid (Abst' val cal)
        err -> err
    where
        restTerm = parseLambdaCal' rest
parseLambdaCal' ((Ident val) : rest) = connectApp (Variable' val) rest
parseLambdaCal' (LeftParen : rest) =
    case right of
        Just pos ->
            case inside of
                Valid item ->
                    connectApp (Paren' item) outside
                err -> err
            where
                inside = parseLambdaCal' (take (pos - 1) rest)
                outside = drop pos rest
        Nothing -> Error (LeftParen : rest)
    where
        right = findRightParen rest
parseLambdaCal' s = Error s

-- Parses a statement.
parseStatement :: [Token] -> ParserResult
parseStatement ((Ident name) : Equal : rest) =
    case parseLambdaCal rest of
        Valid cal -> Valid (FuncDef name cal)
        Error err -> Error err
parseStatement tokens =
    case parseLambdaCal tokens of
        Valid cal -> Valid (Eval cal)
        Error err -> Error err
