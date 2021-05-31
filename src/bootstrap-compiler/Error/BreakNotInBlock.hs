module Error.BreakNotInBlock where

import Global

data Type = Type

instance Show Type where
    show Type = "'break' not in block."
