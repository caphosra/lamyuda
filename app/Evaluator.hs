module Evaluator (
    doSubstituteTerms,
    doBetaReduction
) where

import BetaReduction
import LambdaTerm
import Operation
import Preprocess

--
-- Performs substitution under the context.
-- If the number of recursions exceeds the limit, it stops performing.
--
doSubstituteTerms :: Int -> Context -> Term -> IO Term

doSubstituteTerms 0 _ term = do return term

doSubstituteTerms depth context term
    | substituted /= term = do
        putStrLn $ "= " ++ showTerm substituted
        doSubstituteTerms (depth - 1) context substituted
    | otherwise = return term
    where
        substituted = substBuiltin . substAll context $ term

--
-- Performs beta reduction on the term under the strategy.
-- If the number of recursions exceeds the limit, it stops performing.
--
doBetaReduction :: Int -> ReductionStrategy -> Term -> IO ()

doBetaReduction = doBetaReduction' []
    where
        doBetaReduction' :: [Term] -> Int -> ReductionStrategy -> Term -> IO ()

        doBetaReduction' [] depth strategy term =
            doBetaReduction' [term] depth strategy term

        doBetaReduction' _ 0 _ _ = do
            putStrLn "Maybe diverging."

        doBetaReduction' appeared depth strategy term = do
            case beta strategy term of
                Reduced red -> do
                    putStrLn ("→ " ++ showTerm red)
                    if red `elem` appeared then
                        putStrLn "Diverging."
                    else
                        doBetaReduction' (red : appeared) (depth - 1) strategy red
                NormalForm _ -> putStrLn "Normal form."
