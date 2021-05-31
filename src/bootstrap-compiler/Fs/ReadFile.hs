module Fs.ReadFile where

import Global
import Context.Funs

withDefault = with @ "fs/read-file" $ fun \path -> do
    sendM $ readFile path
