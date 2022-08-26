module Main (main) where

import System.IO
import Control.Monad.IO.Class
import System.Console.Haskeline hiding (outputStrLn)

import BetaReduction
import Evaluator
import LambdaCalculus
import Lexer
import Parser
import Result

main :: IO ()
main =
    do
        hSetEncoding stdin utf8
        hSetEncoding stdout utf8
        putStrLn "Lamdba -- a simple lambda calculus operator"
        runInputT defaultSettings (promptLoop (NormalOrder, []))

type Config = (ReductionStrategy, [(String, LambdaCal)])

promptLoop :: Config -> InputT IO ()
promptLoop config = do
    rawInput <- getInputLine "> "
    case rawInput of
        Just input ->
            tokenizerProc input
                $ parserProc
                $ evalProc config promptLoop
        Nothing -> return ()

outputStrLn :: String -> InputT IO ()
outputStrLn s = do liftIO $ putStrLn s

tokenizerProc :: String -> ([(Int, Token)] -> InputT IO ()) -> InputT IO ()
tokenizerProc input postProc = do
    case tokenize input of
        Valid tokens -> postProc tokens
        Error (pos, c) ->
            outputStrLn ("Lexer error: An invalid character \"" ++ c ++ "\" was found at " ++ show pos)

parserProc :: (Statement -> InputT IO ()) -> [(Int, Token)] -> InputT IO ()
parserProc postProc tokens = do
    case parseStatement tokens of
        Valid st -> postProc st
        Error (pos, c) ->
            outputStrLn ("Syntax error: An unexpected token \"" ++ toStr c ++ "\" was found at " ++ show pos)

evalProc :: Config -> (Config -> InputT IO ()) -> Statement -> InputT IO ()
evalProc (strategy, functions) exec (FuncDef name term)
    | any (\f -> fst f == name) functions = do
        outputStrLn ("\"" ++ name ++ "\" was already defined.")
        outputStrLn ("Previous : " ++ name ++ " = " ++ showLambdaCal prevTerm)
        outputStrLn ("Redefined: " ++ name ++ " = " ++ showLambdaCal term)
        exec (strategy, replaced)
    | otherwise = do
        outputStrLn ("Defined: " ++ name ++ " = " ++ showLambdaCal term)
        exec (strategy, (name, term) : functions)
    where
        prevTerm = snd (head (filter (\f -> fst f == name) functions))
        replaced = map (\f -> if fst f == name then (name, term) else f) functions
evalProc (strategy, functions) exec (Eval cal) = do
    outputStrLn (showLambdaCal cal)
    replaced <- liftIO $ replaceFunction 0 cal functions
    liftIO $ betaReduction (beta strategy) replaced [replaced]
    exec (strategy, functions)
evalProc (strategy, functions) exec (Exec ["list"]) = do
    printFunctions functions
    exec (strategy, functions)
evalProc (_, functions) exec (Exec ["strategy", "no"]) = do
    outputStrLn "Strategy : Normal Order"
    exec (NormalOrder, functions)
evalProc (_, functions) exec (Exec ["strategy", "cn"]) = do
    outputStrLn "Strategy : Call by Name"
    exec (CallByName, functions)
evalProc (_, functions) exec (Exec ["strategy", "cv"]) = do
    outputStrLn "Strategy : Call by Value"
    exec (CallByValue, functions)
evalProc _ _ (Exec ["exit"]) = do
    outputStrLn "Quit."
evalProc config exec _ = do
    outputStrLn "Invalid command."
    exec config

printFunctions :: [(String, LambdaCal)] -> InputT IO ()
printFunctions [] = pure ()
printFunctions ((name, func) : rest) = do
    outputStrLn (name ++ " = " ++ showLambdaCal func)
    printFunctions rest
