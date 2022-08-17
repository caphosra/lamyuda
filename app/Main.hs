module Main (main) where

import LambdaCalculus
import Lexer
import Operation
import Parser
import Result

main :: IO ()
main =
    do
        case tokenize "(lambda n. lambda m. lambda s. lambda z. n s (m s z)) 5 6" of
            Valid tokens ->
                do
                    case parseLambdaCal tokens of
                        Valid cal ->
                            do
                                putStrLn "---- the normal order strategy ----"
                                putStrLn (showLambdaCal cal)
                                printProcesses betaNO (replaceBuiltin cal) []
                                putStrLn "---- the call by name strategy ----"
                                putStrLn (showLambdaCal cal)
                                printProcesses betaCN (replaceBuiltin cal) []
                                putStrLn "---- the call by value strategy ----"
                                putStrLn (showLambdaCal cal)
                                printProcesses betaCV (replaceBuiltin cal) []
                        Error err -> print err
            Error err -> print err

printProcesses :: (LambdaCal -> LambdaCal) -> LambdaCal -> [LambdaCal] -> IO ()
printProcesses beta cal appeared =
    do
        putStrLn (showLambdaCal cal)
        if cal `elem` appeared then
            pure ()
        else
            do
                putStr "→ "
                printProcesses beta (beta cal) (cal : appeared)
