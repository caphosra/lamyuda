module Evaluator (
    doSubstituteTerms,
    doReduction,
) where

import LambdaTerm
import Operation
import Preprocess
import Reduction

--
-- Performs substitution under the context.
-- If the number of recursions exceeds the limit, it stops performing.
--
doSubstituteTerms :: Int -> Context -> (Term -> String) -> Term -> IO Term
doSubstituteTerms 0 _ _ term = do return term
doSubstituteTerms depth context printTerm term
    | substituted /= term = do
        putStrLn $ "= " ++ printTerm substituted
        doSubstituteTerms (depth - 1) context printTerm substituted
    | otherwise = return term
  where
    substituted = substBuiltin . substAll context $ term

--
-- Performs beta reduction on the term under the strategy.
-- If the number of recursions exceeds the limit, it stops performing.
--
doReduction :: Int -> ReductionKind -> ReductionStrategy -> (Term -> String) -> Term -> IO ()
doReduction = doReduction' []
  where
    doReduction' :: [Term] -> Int -> ReductionKind -> ReductionStrategy -> (Term -> String) -> Term -> IO ()

    doReduction' [] depth kind strategy printTerm term =
        doReduction' [term] depth kind strategy printTerm term
    doReduction' _ 0 _ _ _ _ = do
        putStrLn "Maybe diverging."
    doReduction' appeared depth kind strategy printTerm term = do
        case beta strategy term of
            Reduced red -> do
                putStrLn ("→β " ++ printTerm red)
                if red `elem` appeared
                    then putStrLn "Diverging."
                    else doReduction' (red : appeared) (depth - 1) kind strategy printTerm red
            _ ->
                case (kind, eta term) of
                    (BetaEta, Reduced red) -> do
                        putStrLn ("→η " ++ printTerm red)
                        if red `elem` appeared
                            then putStrLn "Diverging."
                            else doReduction' (red : appeared) (depth - 1) kind strategy printTerm red
                    _ -> putStrLn "Normal form."
