module Result (
    Result (
        Valid,
        Error
    ),
    mapResult
) where

--
-- Holds a result of the processing.
--
data Result ok err
    = Valid ok
    | Error err
    deriving (Eq, Show)

--
-- Applies a function to the result. If it is `Error`, do nothing.
--
mapResult :: Result ok err -> (ok -> ok) -> Result ok err

mapResult (Valid v) f = Valid (f v)

mapResult err _ = err
