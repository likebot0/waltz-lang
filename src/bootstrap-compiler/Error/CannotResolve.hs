module Error.CannotResolve where

import Global

newtype Type = Type String

instance Show Type where
    show (Type identifier) = "Cannot resolve '" ++ identifier ++ "'"
