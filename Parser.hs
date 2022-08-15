module Parser (
    LambdaCal (
        Variable,
        Abst,
        App
    ),
    Statement (
        FuncDef,
        Eval
    ),
    parseLambdaCal,
    parseStatement
) where

import Lexer
import Data.List (elemIndex)

data LambdaCal
    = Variable String
    | Abst String LambdaCal
    | App LambdaCal LambdaCal
    | Paren LambdaCal
    deriving (Eq, Show)

data Statement
    = FuncDef String LambdaCal
    | Eval LambdaCal
    deriving (Eq, Show)

type ParseCalResult = Result LambdaCal [Token]
type ParseResult = Result Statement [Token]

addParenNum :: Int -> Token -> Int
addParenNum n LeftParen = 1 + n
addParenNum n RightParen = -1 + n
addParenNum n _ = n

findRightParen :: [Token] -> Maybe Int
findRightParen s = elemIndex 0 (scanl addParenNum 1 s)

connectApp :: LambdaCal -> [Token] -> ParseCalResult
connectApp new [] = Valid new
connectApp new rest =
    case parseLambdaCal rest of
        Valid (App left right) -> Valid (App (App new left) right)
        Valid cal -> Valid (App new cal)
        err -> err

parseLambdaCal :: [Token] -> ParseCalResult
parseLambdaCal s =
    case s of
        Lambda : (Ident val) : Sep : rest ->
            case restTerm of
                Valid cal -> Valid (Abst val cal)
                err -> err
            where
                restTerm = parseLambdaCal rest
        (Ident val) : rest ->
            connectApp (Variable val) rest
        LeftParen : rest ->
            case right of
                Just pos ->
                    case inside of
                        Valid item ->
                            connectApp (Paren item) outside
                        err -> err
                    where
                        inside = parseLambdaCal (take (pos - 1) rest)
                        outside = drop pos rest
                Nothing -> Error s
            where
                right = findRightParen rest
        _ -> Error s

parseStatement :: [Token] -> ParseResult
parseStatement ((Ident name) : Equal : rest) =
    case parseLambdaCal rest of
        Valid cal -> Valid (FuncDef name cal)
        Error err -> Error err
parseStatement tokens =
    case parseLambdaCal tokens of
        Valid cal -> Valid (Eval cal)
        Error err -> Error err
