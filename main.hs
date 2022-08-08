import ArithNumber

printNum :: ArithNumber -> IO ()
printNum num =
    do
        putStrLn "-----"
        putStrLn ("Before: " ++ show num)
        putStrLn ("After : " ++ show (eval num))
        putStrLn ("Number: " ++ show (toNum num))

main =
    do
        printNum (Succ (Succ Zero))
        printNum (Pred Zero)
        printNum (Succ (Pred (Pred Zero)))
