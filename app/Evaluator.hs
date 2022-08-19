module Evaluator (
    evaluateStatement
) where

import LambdaCalculus
import Operation
import Parser
import Preprocess

replaceFunction :: Int -> LambdaCal -> [(String, LambdaCal)] -> IO LambdaCal
replaceFunction depth cal predefined
    | depth > 5 = do
        putStrLn "Too much recursive! Interrupted."
        pure cal
    | replaced /= cal = do
        putStrLn ("= " ++ showLambdaCal replaced)
        replaceFunction (depth + 1) replaced predefined
    | otherwise = pure cal
    where
        replaced = (foldl (flip (uncurry replaceVariable)) . replaceBuiltin) cal predefined

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
    replaced <- replaceFunction 0 cal predefined
    betaReduction betaNO replaced []
    pure predefined
