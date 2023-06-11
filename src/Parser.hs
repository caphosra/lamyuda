module Parser (
    Statement (
        FuncDef,
        Eval,
        Exec
    ),
    parseTerm,
    parseStatement,
) where

import Data.List (elemIndex)

import LambdaTerm
import Lexer
import Result

--
-- A representation of a lambda term with parentheses.
--
data Term'
    = Variable' String -- x
    | Abst' String Term' -- λx. M
    | App' Term' Term' -- M M
    | Paren' Term' -- (M)
    deriving (Eq, Show)

--
-- A representation of a statement.
--
data Statement
    = FuncDef String Term
    | Eval Term
    | Exec [String]
    deriving (Eq, Show)

--
-- Holds a result of parsing a lambda term.
--
type TermParserResult = Result Term (Int, Token)

--
-- Holds a result of parsing a lambda term with parentheses.
--
type TermParserResult' = Result Term' (Int, Token)

--
-- Holds a result of parsing.
--
type ParserResult = Result Statement (Int, Token)

--
-- Parses a lambda term.
--
parseTerm :: [(Int, Token)] -> TermParserResult
parseTerm tokens =
    case parseTerm' tokens of
        Valid term -> Valid (removeParen term)
        Error err -> Error err
  where
    --
    -- Extends a chain of an application, that is "M M M...".
    --
    appendApps :: Term' -> Term' -> Term'

    appendApps new (App' left right) = App' (appendApps new left) right
    appendApps new term = App' new term

    --
    -- Removes parentheses from a lambda term.
    --
    removeParen :: Term' -> Term

    removeParen (Variable' name) = Variable name
    removeParen (Abst' name term) = Abst name (removeParen term)
    removeParen (App' left right) = App (removeParen left) (removeParen right)
    removeParen (Paren' term) = removeParen term

    --
    -- Parses an application.
    --
    parseApp :: Term' -> [(Int, Token)] -> TermParserResult'

    parseApp new [] = Valid new
    parseApp new rest =
        case parseTerm' rest of
            Valid term -> Valid (appendApps new term)
            err -> err

    --
    -- Parses a lambda term with parentheses.
    --
    parseTerm' :: [(Int, Token)] -> TermParserResult'

    parseTerm' ((_, Lambda) : (_, Ident val) : (_, Sep) : rest) =
        mapResult (parseTerm' rest) (Abst' val)
    parseTerm' ((_, Ident val) : rest) = parseApp (Variable' val) rest
    parseTerm' ((leftPos, LeftParen) : rest) =
        case right of
            Just pos ->
                case inside of
                    Valid item ->
                        parseApp (Paren' item) outside
                    err -> err
              where
                inside = parseTerm' (take (pos - 1) rest)
                outside = drop pos rest
            Nothing -> Error (leftPos, LeftParen)
      where
        right = findRightParen rest

        --
        -- Looks for the corresponding right parenthesis.
        --
        findRightParen :: [(Int, Token)] -> Maybe Int

        findRightParen s = elemIndex 0 (scanl addParenNum 1 s)
          where
            --
            -- Adds +1 if the token is "(" and -1 if ")".
            --
            addParenNum :: Int -> (Int, Token) -> Int

            addParenNum n (_, LeftParen) = 1 + n
            addParenNum n (_, RightParen) = -1 + n
            addParenNum n _ = n
    parseTerm' [] = Error (0, Ident "")
    parseTerm' s = Error (head s)

--
-- Parses a statement.
--
parseStatement :: [(Int, Token)] -> ParserResult
parseStatement ((_, Ident name) : (_, Equal) : rest) =
    case parseTerm rest of
        Valid term -> Valid (FuncDef name term)
        Error err -> Error err
parseStatement ((_, Command) : rest) = Valid (Exec (map (show . snd) rest))
parseStatement tokens =
    case parseTerm tokens of
        Valid term -> Valid (Eval term)
        Error err -> Error err
