module Diagnostic.CannotResolve where

import Global
import Context.Funs
import Ast
import Ast.Syntax
import Diagnostic

send x =
    call @ "diagnostic/send" $ Diagnostic.Diagnostic
        do inject $ Proxy @ "error"
        do Ast.Syntax.location $ Ast.attributes x
        do "Cannot resolve '" ++ Ast.children x ++ "'"
