module Result (
    Result (
        Valid,
        Error
    )
) where

-- Holds a result of the processing.
data Result item err
    = Valid item
    | Error err
    deriving (Eq, Show)
