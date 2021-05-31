module Action.GetOutDir where

import Global
import Context.Funs

withDefault = with @ "get-out-dir" $ fun \x -> do
    return "./out"
