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
    applyDiff,
) where

import Data.Tuple.All

import LambdaTerm
import Reduction

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
type Config = (ReductionStrategy, ReductionKind, Context, Term -> String)

--
-- A modification of the configuration.
--
type ConfigMod = (Diff ReductionStrategy, Diff ReductionKind, Diff Context, Diff (Term -> String))

--
-- Returns a modification that holds "no difference".
--
unmodified :: ConfigMod
unmodified = (Unmodified, Unmodified, Unmodified, Unmodified)

--
-- Converts a configuration into a diff.
--
toConfigMod :: Config -> ConfigMod
toConfigMod (strategy, kind, context, printTerm) =
    (Modified strategy, Modified kind, Modified context, Modified printTerm)

--
-- The default configuration.
--
defaultConfig :: Config
defaultConfig = (NormalOrder, BetaOnly, [], showTerm)

--
-- Applies a modification to the configuration used.
--
applyDiff :: ConfigMod -> Config -> Config
applyDiff modification =
    case modification of
        (Modified strategy, _, _, _) ->
            applyDiff (upd1 Unmodified modification) . upd1 strategy
        (_, Modified kind, _, _) ->
            applyDiff (upd2 Unmodified modification) . upd2 kind
        (_, _, Modified context, _) ->
            applyDiff (upd3 Unmodified modification) . upd3 context
        (_, _, _, Modified printTerm) ->
            applyDiff (upd4 Unmodified modification) . upd4 printTerm
        _ -> id
