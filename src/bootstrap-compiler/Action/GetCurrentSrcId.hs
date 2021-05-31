module Action.GetCurrentSrcId where

import Global
import Context.Funs

withDefault = with @ "get-current-src-id" $ fun \x -> do
    return ""
