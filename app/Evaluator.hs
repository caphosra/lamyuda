module Evaluator (
    evaluateStatement
) where

import LambdaCalculus
import Operation
import Parser
import Preprocess

betaReduction :: (LambdaCal -> LambdaCal) -> LambdaCal -> [LambdaCal] -> IO ()
betaReduction beta cal appeared = do
    if cal `elem` appeared then
        pure ()
    else
        do
            putStrLn ("→ " ++ showLambdaCal (beta cal))
            betaReduction beta (beta cal) (cal : appeared)

evaluateStatement :: Statement -> [(String, LambdaCal)] -> IO [(String, LambdaCal)]
evaluateStatement (FuncDef name cal) predefined = do
    putStrLn ("Defined " ++ name ++ " = " ++ showLambdaCal cal)
    pure ((name, cal) : predefined)
evaluateStatement (Eval cal) predefined = do
    putStrLn (showLambdaCal cal)
    if replaced /= cal then
        putStrLn ("= " ++ showLambdaCal replaced)
    else
        pure ()
    betaReduction betaNO replaced []
    pure predefined
    where
        replaced = (foldl (flip (uncurry replaceVariable)) . replaceBuiltin) cal predefined
