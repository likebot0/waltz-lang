module Action.GetEntrypoints where

import Global
import Context.Funs
import qualified System.Environment

withDefault = with @ "get-entrypoints" $ fun \x -> do
    sendM System.Environment.getArgs
