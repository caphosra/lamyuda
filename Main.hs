import Lexer
import Parser
import Operation

main =
    do
        case tokenize "(lambda n. lambda m. lambda s. lambda z. n s (m s z)) (lambda s. lambda z. s z) (lambda s. lambda z. s z)" of
            Valid tokens ->
                do
                    case parseLambdaCal tokens of
                        Valid cal ->
                            do
                                putStrLn "---- the normal order strategy ----"
                                printProcesses betaNO cal []
                                putStrLn "---- the call by name strategy ----"
                                printProcesses betaCN cal []
                                putStrLn "---- the call by value strategy ----"
                                printProcesses betaCV cal []
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
