module Evaluator (
    replaceFunction,
    betaReduction
) where

import BetaReduction
import LambdaCalculus
import Operation
import Preprocess

replaceFunction :: Int -> LambdaCal -> [(String, LambdaCal)] -> IO LambdaCal
replaceFunction 0 cal _ = do pure cal
replaceFunction depth cal predefined
    | replaced /= cal = do
        putStrLn ("= " ++ showLambdaCal replaced)
        replaceFunction (depth - 1) replaced predefined
    | otherwise = pure cal
    where
        replaced = (replaceBuiltin . replaceAll predefined) cal

betaReduction :: Int -> (LambdaCal -> ReductionResult) -> LambdaCal -> [LambdaCal] -> IO ()
betaReduction 0 _ _ _ = do
    putStrLn "Maybe diverging."
betaReduction depth betaRed cal appeared = do
    case betaRed cal of
        Reduced red -> do
            putStrLn ("→ " ++ showLambdaCal red)
            if red `elem` appeared then
                putStrLn "Diverging."
            else
                betaReduction (depth - 1) betaRed red (red : appeared)
        NormalForm _ -> putStrLn "Normal form."
