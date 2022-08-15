module Parser (
    LambdaCal (
        Variable,
        Abst,
        App
    ),
    findRightParen,
    parseLambdaCal
) where

import ArithTerm
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

type ParseResult = Result LambdaCal [Token]

addParenNum :: Int -> Token -> Int
addParenNum n LeftParen = 1 + n
addParenNum n RightParen = -1 + n
addParenNum n _ = n

findRightParen :: [Token] -> Maybe Int
findRightParen s = elemIndex 0 (scanl addParenNum 1 s)

connectApp :: LambdaCal -> [Token] -> ParseResult
connectApp new [] = Valid new
connectApp new rest =
    case parseLambdaCal rest of
        Valid (App left right) -> Valid (App (App new left) right)
        Valid cal -> Valid (App new cal)
        err -> err

parseLambdaCal :: [Token] -> ParseResult
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
