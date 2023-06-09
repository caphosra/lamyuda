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

import Reduction
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
type Config = (ReductionStrategy, ReductionKind, Context)

--
-- A modification of the configuration.
--
type ConfigMod = (Diff ReductionStrategy, Diff ReductionKind, Diff Context)

--
-- Returns a modification that holds "no difference".
--
unmodified :: ConfigMod

unmodified = (Unmodified, Unmodified, Unmodified)

--
-- Converts a configuration into a diff.
--
toConfigMod :: Config -> ConfigMod

toConfigMod (strategy, kind, context) =
    (Modified strategy, Modified kind, Modified context)

--
-- The default configuration.
--
defaultConfig :: Config

defaultConfig = (NormalOrder, BetaOnly, [])

--
-- Applies a modification to the configuration used.
--
applyDiff :: ConfigMod -> Config -> Config

applyDiff modification =
    case modification of
        (Modified strategy, _, _) ->
            applyDiff (upd1 Unmodified modification) . upd1 strategy
        (_, Modified kind, _) ->
            applyDiff (upd2 Unmodified modification) . upd2 kind
        (_, _, Modified context) ->
            applyDiff (upd3 Unmodified modification) . upd3 context
        _ -> id
