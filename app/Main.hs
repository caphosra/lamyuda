module Main (main) where

import System.IO

import BetaReduction
import Evaluator
import LambdaCalculus
import Lexer
import Parser
import Result

main :: IO ()
main =
    do
        putStrLn "Lamdba -- a simple lambda calculus operator"
        prompt (NormalOrder, [])

type Config = (ReductionStrategy, [(String, LambdaCal)])

prompt :: Config -> IO ()
prompt predefined = do
    putStr "> "
    hFlush stdout
    input <- getLine
    tokenizerProc input
        $ parserProc
        $ evalProc predefined prompt

tokenizerProc :: String -> ([(Int, Token)] -> IO ()) -> IO ()
tokenizerProc input postProc = do
    case tokenize input of
        Valid tokens -> postProc tokens
        Error (pos, c) ->
            putStrLn ("Lexer error: An invalid character \"" ++ c ++ "\" was found at " ++ show pos)

parserProc :: (Statement -> IO ()) -> [(Int, Token)] -> IO ()
parserProc postProc tokens = do
    case parseStatement tokens of
        Valid st -> postProc st
        Error (pos, c) ->
            putStrLn ("Syntax error: An unexpected token \"" ++ toStr c ++ "\" was found at " ++ show pos)

evalProc :: Config -> (Config -> IO ()) -> Statement -> IO ()
evalProc (strategy, functions) exec (FuncDef name term)
    | any (\f -> fst f == name) functions = do
        putStrLn ("\"" ++ name ++ "\" was already defined.")
        putStrLn ("Previous : " ++ name ++ " = " ++ showLambdaCal prevTerm)
        putStrLn ("Redefined: " ++ name ++ " = " ++ showLambdaCal term)
        exec (strategy, replaced)
    | otherwise = do
        putStrLn ("Defined: " ++ name ++ " = " ++ showLambdaCal term)
        exec (strategy, (name, term) : functions)
    where
        prevTerm = snd (head (filter (\f -> fst f == name) functions))
        replaced = map (\f -> if fst f == name then (name, term) else f) functions
evalProc (strategy, functions) exec (Eval cal) = do
    putStrLn (showLambdaCal cal)
    replaced <- replaceFunction 0 cal functions
    betaReduction (beta strategy) replaced [replaced]
    exec (strategy, functions)
evalProc (strategy, functions) exec (Exec ["list"]) = do
    printFunctions functions
    exec (strategy, functions)
evalProc (_, functions) exec (Exec ["strategy", "no"]) = do
    putStrLn "Strategy : Normal Order"
    exec (NormalOrder, functions)
evalProc (_, functions) exec (Exec ["strategy", "cn"]) = do
    putStrLn "Strategy : Call by Name"
    exec (CallByName, functions)
evalProc (_, functions) exec (Exec ["strategy", "cv"]) = do
    putStrLn "Strategy : Call by Value"
    exec (CallByValue, functions)
evalProc _ _ (Exec ["exit"]) = do
    putStrLn "Quit."
evalProc config exec _ = do
    putStrLn "Invalid command."
    exec config

printFunctions :: [(String, LambdaCal)] -> IO ()
printFunctions [] = pure ()
printFunctions ((name, func) : rest) = do
    putStrLn (name ++ " = " ++ showLambdaCal func)
    printFunctions rest
