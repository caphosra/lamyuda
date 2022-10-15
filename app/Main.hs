module Main (main) where

import Control.Monad.IO.Class
import Data.List.Split
import System.Console.Haskeline
import System.IO

import BetaReduction
import Configuration
import Evaluator
import LambdaTerm
import Lexer
import Parser
import Result

main :: IO ()

main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    putStrLn "Lamdba -- a simple lambda terms operator"
    runInputT defaultSettings (promptLoop defaultConfig)

--
-- Receives an input and evaluates it if valid.
--
promptLoop :: Config -> InputT IO ()

promptLoop config = do
    rawInput <- getInputLine "> "
    case rawInput of
        Just input -> do
            result <- liftIO
                $ doTokenize input
                $ doParse
                $ doEvaluate config
            case result of
                KeepAlive diff ->
                    promptLoop $ applyDiff diff config
                Quit -> return ()
        Nothing -> return ()

--
-- Tokenizes the input. If succeed, it executes `post`.
--
doTokenize :: String -> ([(Int, Token)] -> IO PromptResult) -> IO PromptResult

doTokenize input post = do
    case tokenize input of
        Valid tokens -> post tokens
        Error (pos, chr) -> do
            putStrLn $ "Lexer error: An invalid character \"" ++ chr ++ "\" was found at " ++ show pos
            return $ KeepAlive unmodified

--
-- Parses the stream of tokens into an AST or a command. If succeed, it executes `post`.
--
doParse :: (Statement -> IO PromptResult) -> [(Int, Token)] -> IO PromptResult

doParse post tokens = do
    case parseStatement tokens of
        Valid stmt -> post stmt
        Error (pos, token) -> do
            putStrLn $ "Syntax error: An unexpected token \"" ++ show token ++ "\" was found at " ++ show pos
            return $ KeepAlive unmodified

--
-- Evaluates the AST. That can be a command started with a character "#".
--
doEvaluate :: Config -> Statement -> IO PromptResult

doEvaluate (_, context) (FuncDef name term)
    | any ((== name) . fst) context = do
        putStrLn $ "\"" ++ name ++ "\" was already defined."
        putStrLn $ "Previous : " ++ name ++ " = " ++ showTerm prevTerm
        putStrLn $ "Redefined: " ++ name ++ " = " ++ showTerm term
        return $ KeepAlive (Unmodified, Modified updated)
    | otherwise = do
        putStrLn $ "Defined: " ++ name ++ " = " ++ showTerm term
        return $ KeepAlive (Unmodified, Modified updated)
    where
        prevTerm = snd $ head $ filter ((== name) . fst) context
        updated = (name, term) : filter ((/= name) . fst) context

doEvaluate (strategy, context) (Eval term) = do
    putStrLn (showTerm term)
    substituted <- liftIO $ doSubstituteTerms 5 context term
    liftIO $ doBetaReduction 30 strategy substituted
    return $ KeepAlive unmodified

doEvaluate (_, []) (Exec ["list"]) = do
    putStrLn "No terms defined in the context."
    return $ KeepAlive unmodified

doEvaluate (_, context) (Exec ["list"]) = do
    printContext context
    return $ KeepAlive unmodified
    where
        printContext :: Context -> IO ()

        printContext [] = return ()

        printContext ((name, term) : rest) = do
            putStrLn $ name ++ " = " ++ showTerm term
            printContext rest

doEvaluate (NormalOrder, _) (Exec ["strategy"]) = do
    putStrLn "Current strategy : Normal Order"
    return $ KeepAlive unmodified

doEvaluate (CallByName, _) (Exec ["strategy"]) = do
    putStrLn "Current strategy : Call by Name"
    return $ KeepAlive unmodified

doEvaluate (CallByValue, _) (Exec ["strategy"]) = do
    putStrLn "Current strategy : Call by Value"
    return $ KeepAlive unmodified

doEvaluate (_, _) (Exec ["strategy", "no"]) = do
    putStrLn "Strategy : Normal Order"
    return $ KeepAlive (Modified NormalOrder, Unmodified)

doEvaluate (_, _) (Exec ["strategy", "cn"]) = do
    putStrLn "Strategy : Call by Name"
    return $ KeepAlive (Modified CallByName, Unmodified)

doEvaluate (_, _) (Exec ["strategy", "cv"]) = do
    putStrLn "Strategy : Call by Value"
    return $ KeepAlive (Modified CallByValue, Unmodified)

doEvaluate config (Exec ["eval", source]) = do
    content <- readFile path
    newConfig <- evalOnce config $ splitOn "\n" content
    return $ KeepAlive $ toConfigMod newConfig
    where
        path = take (length source - 2) $ drop 1 source

        evalOnce :: Config -> [String] -> IO Config

        evalOnce newConfig [] = do return newConfig

        evalOnce newConfig (input : rest) = do
            putStrLn $ ">> " ++ input
            result <-
                doTokenize input
                $ doParse
                $ doEvaluate newConfig
            case result of
                KeepAlive diff ->
                    evalOnce (applyDiff diff newConfig) rest
                Quit -> return newConfig

doEvaluate _ (Exec ["exit"]) = do
    putStrLn "Quit."
    return Quit

doEvaluate _ _ = do
    putStrLn "Invalid command."
    return $ KeepAlive unmodified
