module Main (main) where

import System.IO

import Evaluator
import LambdaCalculus
import Lexer
import Parser
import Result

main :: IO ()
main =
    do
        putStrLn "Lamdba -- a simple lambda calculus operator"
        _ <- prompt []
        pure ()

prompt :: [(String, LambdaCal)] -> IO [(String, LambdaCal)]
prompt predefined = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if input /= "exit" then
        case tokenize input of
            Valid tokens ->
                case parseStatement tokens of
                    Valid st -> do
                        newPredefined <- evaluateStatement st predefined
                        prompt newPredefined
                    Error err -> do
                        putStrLn ("Syntax error \"" ++ show err ++ "\"")
                        prompt predefined
            Error err -> do
                putStrLn ("Invalid character found \"" ++ err ++ "\"")
                prompt predefined
    else
        do
            putStrLn "Quit"
            pure predefined
