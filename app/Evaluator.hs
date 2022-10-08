module Evaluator (
    doSubstituteTerms,
    doBetaReduction
) where

import BetaReduction
import Configuration
import LambdaCalculus
import Operation
import Preprocess

--
-- Performs substitution under the context.
-- If the number of recursions exceeds the limit, it stops performing.
--
doSubstituteTerms :: Int -> Context -> LambdaCal -> IO LambdaCal

doSubstituteTerms 0 _ term = do return term

doSubstituteTerms depth context term
    | substituted /= term = do
        putStrLn $ "= " ++ showLambdaCal substituted
        doSubstituteTerms (depth - 1) context substituted
    | otherwise = return term
    where
        substituted = replaceBuiltin . replaceAll context $ term

--
-- Performs beta reduction on the term under the strategy.
-- If the number of recursions exceeds the limit, it stops performing.
--
doBetaReduction :: Int -> ReductionStrategy -> LambdaCal -> IO ()

doBetaReduction = doBetaReduction' []
    where
        doBetaReduction' :: [LambdaCal] -> Int -> ReductionStrategy -> LambdaCal -> IO ()

        doBetaReduction' [] depth strategy term =
            doBetaReduction' [term] depth strategy term

        doBetaReduction' _ 0 _ _ = do
            putStrLn "Maybe diverging."

        doBetaReduction' appeared depth strategy term = do
            case beta strategy term of
                Reduced red -> do
                    putStrLn ("→ " ++ showLambdaCal red)
                    if red `elem` appeared then
                        putStrLn "Diverging."
                    else
                        doBetaReduction' (red : appeared) (depth - 1) strategy red
                NormalForm _ -> putStrLn "Normal form."
