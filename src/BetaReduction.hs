module BetaReduction (
    ReductionResult (
        Reduced,
        NormalForm
    ),
    ReductionStrategy (
        NormalOrder,
        CallByName,
        CallByValue
    ),
    beta,
) where

import LambdaCalculus
import Operation

--
-- Holds a result of beta-reduction.
--
data ReductionResult
    = Reduced LambdaCal
    | NormalForm LambdaCal

--
-- A strategy conducting beta-reduction repeatedly.
--
data ReductionStrategy
    = NormalOrder
    | CallByName
    | CallByValue

--
-- Applies a function to the term of the result.
--
mapResult :: ReductionResult -> (LambdaCal -> LambdaCal) -> ReductionResult

mapResult (Reduced term) f = Reduced (f term)

mapResult (NormalForm term) f = NormalForm (f term)

--
-- Conducts beta-reduction.
--
beta :: ReductionStrategy -> LambdaCal -> ReductionResult

beta NormalOrder = betaNO

beta CallByName = betaCN

beta CallByValue = betaCV

--
-- Conducts beta-reduction with the normal form strategy.
--
betaNO :: LambdaCal -> ReductionResult

betaNO (Abst name child) = mapResult (betaNO child) (Abst name)

betaNO (App (Abst name child) arg) = Reduced (replaceVariable name arg child)

betaNO (App left right) =
    case betaNO left of
        Reduced red -> Reduced (App red right)
        NormalForm _ -> mapResult (betaNO right) (App left)

betaNO term = NormalForm term

--
-- Conducts beta-reduction with the call by name strategy.
--
betaCN :: LambdaCal -> ReductionResult

betaCN (App (Abst name child) arg) = Reduced (replaceVariable name arg child)

betaCN (App left right) =
    case betaCN left of
        Reduced red -> Reduced (App red right)
        NormalForm _ -> mapResult (betaCN right) (App left)

betaCN term = NormalForm term

--
-- Conducts beta-reduction with the call by value strategy.
--
betaCV :: LambdaCal -> ReductionResult

betaCV (App (Abst name1 child1) (Abst name2 child2)) =
    Reduced (replaceVariable name1 (Abst name2 child2) child1)

betaCV (App left right) =
    case betaCV left of
        Reduced red -> Reduced (App red right)
        NormalForm _ -> mapResult (betaCV right) (App left)

betaCV term = NormalForm term
