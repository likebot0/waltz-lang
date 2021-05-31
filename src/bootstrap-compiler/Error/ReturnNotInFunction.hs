module Error.ReturnNotInFunction where

import Global

data Type = Type

instance Show Type where
    show Type = "'return' not in function."
