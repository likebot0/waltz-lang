module Diagnostic.UnknownField where

import Global
import Context.Funs
import Ast
import Ast.Syntax
import Diagnostic

send x name =
    call @ "diagnostic/send" $ Diagnostic.Diagnostic
        do inject $ Proxy @ "error"
        x
        do "Unknown field '" ++ name ++ "' on type object"
