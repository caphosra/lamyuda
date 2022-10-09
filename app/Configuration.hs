module Configuration (
    Diff (
        Modified,
        Unmodified
    ),
    PromptResult (
        KeepAlive,
        Quit
    ),
    Config,
    ConfigMod,
    unmodified,
    toConfigMod,
    defaultConfig,
    applyDiff
) where

import Data.Tuple.All

import BetaReduction
import LambdaTerm

--
-- Represents a difference if exists.
--
data Diff a
    = Modified a
    | Unmodified

--
-- The result of the prompt.
--
data PromptResult
    = KeepAlive ConfigMod
    | Quit

--
-- A configuration of the prompt.
--
type Config = (ReductionStrategy, Context)

--
-- A modification of the configuration.
--
type ConfigMod = (Diff ReductionStrategy, Diff Context)

--
-- Returns a modification that holds "no difference".
--
unmodified :: ConfigMod

unmodified = (Unmodified, Unmodified)

--
-- Converts a configuration into a diff.
--
toConfigMod :: Config -> ConfigMod

toConfigMod (strategy, context) =
    (Modified strategy, Modified context)

--
-- The default configuration.
--
defaultConfig :: Config

defaultConfig = (NormalOrder, [])

--
-- Applies a modification to the configuration used.
--
applyDiff :: ConfigMod -> Config -> Config

applyDiff modification =
    case modification of
        (Modified strategy, _) ->
            applyDiff (upd1 Unmodified modification) . upd1 strategy
        (_, Modified context) ->
            applyDiff (upd2 Unmodified modification) . upd2 context
        _ -> id
