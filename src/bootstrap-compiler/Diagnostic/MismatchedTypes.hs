module Diagnostic.MismatchedTypes where

import Global
import Context.Funs
import Ast
import Ast.Semantic
import Ast.Syntax
import Diagnostic

send x =
    call @ "diagnostic/send" $ Diagnostic.Diagnostic
        do inject $ Proxy @ "error"
        x
        "Mismatched types"
