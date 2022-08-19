module Parser (
    Statement (
        FuncDef,
        Eval,
        Exec
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
    | Exec [String]
    deriving (Eq, Show)

-- Holds a result of parsing a lambda calculus.
type CalParserResult = Result LambdaCal (Int, Token)

-- Holds a result of parsing a lambda calculus with parentheses.
type CalParserResult' = Result LambdaCal' (Int, Token)

-- Holds a result of parsing.
type ParserResult = Result Statement (Int, Token)

-- Adds +1 if the token is "(" and -1 if ")".
addParenNum :: Int -> (Int, Token) -> Int
addParenNum n (_, LeftParen) = 1 + n
addParenNum n (_, RightParen) = -1 + n
addParenNum n _ = n

-- Looks for the corresponding right parenthesis.
findRightParen :: [(Int, Token)] -> Maybe Int
findRightParen s = elemIndex 0 (scanl addParenNum 1 s)

-- Extends a chain of an application, that is "M M M...".
appendApps :: LambdaCal' -> LambdaCal' -> LambdaCal'
appendApps new (App' left right) = App' (appendApps new left) right
appendApps new cal = App' new cal

-- Removes parentheses from a lambda calculus.
removeParen :: LambdaCal' -> LambdaCal
removeParen (Variable' name) = Variable name
removeParen (Abst' name cal) = Abst name (removeParen cal)
removeParen (App' left right) = App (removeParen left) (removeParen right)
removeParen (Paren' cal) = removeParen cal

-- Parses an application.
parseApp :: LambdaCal' -> [(Int, Token)] -> CalParserResult'
parseApp new [] = Valid new
parseApp new rest =
    case parseLambdaCal' rest of
        Valid cal -> Valid (appendApps new cal)
        err -> err

-- Parses a lambda calculus.
parseLambdaCal :: [(Int, Token)] -> CalParserResult
parseLambdaCal s =
    case parseLambdaCal' s of
        Valid cal -> Valid (removeParen cal)
        Error err -> Error err

-- Parses a lambda calculus with parentheses.
parseLambdaCal' :: [(Int, Token)] -> CalParserResult'
parseLambdaCal' ((_, Lambda) : (_, Ident val) : (_, Sep) : rest) =
    mapResult (parseLambdaCal' rest) (Abst' val)
parseLambdaCal' ((_, Ident val) : rest) = parseApp (Variable' val) rest
parseLambdaCal' ((leftPos, LeftParen) : rest) =
    case right of
        Just pos ->
            case inside of
                Valid item ->
                    parseApp (Paren' item) outside
                err -> err
            where
                inside = parseLambdaCal' (take (pos - 1) rest)
                outside = drop pos rest
        Nothing -> Error (leftPos, LeftParen)
    where
        right = findRightParen rest
parseLambdaCal' [] = Error (0, Ident "")
parseLambdaCal' s = Error (head s)

-- Parses a statement.
parseStatement :: [(Int, Token)] -> ParserResult
parseStatement ((_, Ident name) : (_, Equal) : rest) =
    case parseLambdaCal rest of
        Valid cal -> Valid (FuncDef name cal)
        Error err -> Error err
parseStatement ((_, Command) : rest) = Valid (Exec (map (toStr . snd) rest))
parseStatement tokens =
    case parseLambdaCal tokens of
        Valid cal -> Valid (Eval cal)
        Error err -> Error err
