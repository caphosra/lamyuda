import Lexer
import Parser

parse :: String -> IO ()
parse cal =
    case tokenize cal of
        Valid tokens ->
            do
                putStrLn "-----"
                putStrLn ("Tokens   : " ++ show tokens)
                putStrLn ("Statement: " ++ show (parseStatement tokens))
        Error err -> print err

main =
    do
        parse "exp = lambda n. lambda m. m n"
        parse "succ = lambda n. lambda s. lambda z. n s (s z)"
        parse "lambda x. x lambda x. x (x x)"
