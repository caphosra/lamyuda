import ArithTerm
import Lexer

printTerm :: ArithTerm -> IO ()
printTerm num =
    do
        putStrLn "-----"
        putStrLn ("Before : " ++ show num)
        putStrLn ("After  : " ++ show evaluated)
        putStrLn ("IsValue: " ++ show (isValue evaluated))
    where evaluated = eval num

main =
    do
        print (tokenize "exp = lambda n. lambda m. m n")
        print (tokenize "succ = lambda n. lambda s. lambda z. n s (s z)")

        printTerm (Succ (Pred ATrue))
        printTerm (Succ (Succ Zero))
        printTerm (Pred Zero)
        printTerm (Succ (Pred (Pred Zero)))
        printTerm (If (If ATrue (IsZero (Succ Zero)) ATrue) (Succ (Pred Zero)) (Pred (Succ Zero)))
