module Evaluator (
    doSubstituteTerms,
    doReduction
) where

import Reduction
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
doReduction :: Int -> ReductionKind -> ReductionStrategy -> Term -> IO ()

doReduction = doReduction' []
    where
        doReduction' :: [Term] -> Int -> ReductionKind -> ReductionStrategy -> Term -> IO ()

        doReduction' [] depth kind strategy term =
            doReduction' [term] depth kind strategy term

        doReduction' _ 0 _ _ _ = do
            putStrLn "Maybe diverging."

        doReduction' appeared depth kind strategy term = do
            case beta strategy term of
                Reduced red -> do
                    putStrLn ("→β " ++ showTerm red)
                    if red `elem` appeared
                        then putStrLn "Diverging."
                        else doReduction' (red : appeared) (depth - 1) kind strategy red
                _ ->
                    case (kind, eta term) of
                        (BetaEta, Reduced red) -> do
                            putStrLn ("→η " ++ showTerm red)
                            if red `elem` appeared
                                then putStrLn "Diverging."
                                else doReduction' (red : appeared) (depth - 1) kind strategy red
                        _ -> putStrLn "Normal form."
