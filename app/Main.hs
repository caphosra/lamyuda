module Main (main) where

import Control.Monad.IO.Class
import System.Console.Haskeline
import System.IO

import BetaReduction
import Configuration
import Evaluator
import LambdaCalculus
import Lexer
import Parser
import Result

main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    putStrLn "Lamdba -- a simple lambda calculus operator"
    runInputT defaultSettings (promptLoop defaultConfig)

promptLoop :: Config -> InputT IO ()
promptLoop config = do
    rawInput <- getInputLine "> "
    case rawInput of
        Just input -> do
            result <- liftIO
                $ tokenizerProc input
                $ parserProc
                $ evalProc config
            case result of
                KeepAlive diff ->
                    promptLoop $ applyDiff diff config
                Quit -> return ()
        Nothing -> return ()

tokenizerProc :: String -> ([(Int, Token)] -> IO PromptResult) -> IO PromptResult
tokenizerProc input postProc = do
    case tokenize input of
        Valid tokens -> postProc tokens
        Error (pos, c) -> do
            putStrLn ("Lexer error: An invalid character \"" ++ c ++ "\" was found at " ++ show pos)
            return $ KeepAlive unmodified

parserProc :: (Statement -> IO PromptResult) -> [(Int, Token)] -> IO PromptResult
parserProc postProc tokens = do
    case parseStatement tokens of
        Valid st -> postProc st
        Error (pos, c) -> do
            putStrLn ("Syntax error: An unexpected token \"" ++ toStr c ++ "\" was found at " ++ show pos)
            return $ KeepAlive unmodified

evalProc :: Config -> Statement -> IO PromptResult
evalProc (_, functions) (FuncDef name term)
    | any ((name ==) . fst) functions = do
        putStrLn ("\"" ++ name ++ "\" was already defined.")
        putStrLn ("Previous : " ++ name ++ " = " ++ showLambdaCal prevTerm)
        putStrLn ("Redefined: " ++ name ++ " = " ++ showLambdaCal term)
        return $ KeepAlive (Unmodified, Modified updated)
    | otherwise = do
        putStrLn ("Defined: " ++ name ++ " = " ++ showLambdaCal term)
        return $ KeepAlive (Unmodified, Modified updated)
    where
        prevTerm = snd $ head $ filter ((name ==) . fst) functions
        updated = (name, term) : filter ((name /=) . fst) functions
evalProc (strategy, functions) (Eval cal) = do
    putStrLn (showLambdaCal cal)
    replaced <- liftIO $ replaceFunction 5 cal functions
    liftIO $ betaReduction 30 (beta strategy) replaced [replaced]
    return $ KeepAlive unmodified
evalProc (_, functions) (Exec ["list"]) = do
    printFunctionsList functions
    return $ KeepAlive unmodified
    where
        printFunctionsList :: [(String, LambdaCal)] -> IO ()
        printFunctionsList [] = return ()
        printFunctionsList ((name, func) : rest) = do
            putStrLn (name ++ " = " ++ showLambdaCal func)
            printFunctionsList rest
evalProc (NormalOrder, _) (Exec ["strategy"]) = do
    putStrLn "Current strategy : Normal Order"
    return $ KeepAlive unmodified
evalProc (CallByName, _) (Exec ["strategy"]) = do
    putStrLn "Current strategy : Call by Name"
    return $ KeepAlive unmodified
evalProc (CallByValue, _) (Exec ["strategy"]) = do
    putStrLn "Current strategy : Call by Value"
    return $ KeepAlive unmodified
evalProc (_, _) (Exec ["strategy", "no"]) = do
    putStrLn "Strategy : Normal Order"
    return $ KeepAlive (Modified NormalOrder, Unmodified)
evalProc (_, _) (Exec ["strategy", "cn"]) = do
    putStrLn "Strategy : Call by Name"
    return $ KeepAlive (Modified CallByName, Unmodified)
evalProc (_, _) (Exec ["strategy", "cv"]) = do
    putStrLn "Strategy : Call by Value"
    return $ KeepAlive (Modified CallByValue, Unmodified)
evalProc _ (Exec ["exit"]) = do
    putStrLn "Quit."
    return Quit
evalProc _ _ = do
    putStrLn "Invalid command."
    return $ KeepAlive unmodified
