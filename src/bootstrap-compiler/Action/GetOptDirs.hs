module Action.GetOptDirs where

import Global
import Context.Funs

withDefault = with @ "get-opt-dirs" $ fun \x -> do
    return ["./opt"]
