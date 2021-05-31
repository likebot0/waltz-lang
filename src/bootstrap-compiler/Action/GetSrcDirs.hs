module Action.GetSrcDirs where

import Global
import Context.Funs

withDefault = with @ "get-src-dirs" $ fun \_ -> do
    return ["./src"]
