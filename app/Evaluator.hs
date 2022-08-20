module Evaluator (
    replaceFunction,
    betaReduction
) where

import BetaReduction
import LambdaCalculus
import Operation
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
        replaced = (replaceBuiltin . replaceAll predefined) cal

betaReduction :: (LambdaCal -> ReductionResult) -> LambdaCal -> [LambdaCal] -> IO ()
betaReduction betaRed cal appeared = do
    case betaRed cal of
        Reduced red -> do
            putStrLn ("→ " ++ showLambdaCal red)
            if red `elem` appeared then
                putStrLn "Diverging."
            else
                betaReduction betaRed red (red : appeared)
        NormalForm _ -> putStrLn "Normal form."
