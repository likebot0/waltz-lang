module Ast.Dump where

import Global
import Context.Funs

withDefault = with @ "ast/dump" $ fun \ast -> do
    call @ "ast/dump-json" ast
