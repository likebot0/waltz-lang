module Diagnostic.IndexOutOfBounds where

import Global
import Context.Funs
import Ast
import Ast.Syntax
import Diagnostic

send x i n =
    call @ "diagnostic/send" $ Diagnostic.Diagnostic
        do inject $ Proxy @ "error"
        x
        do "Index '" ++ i ++ "' is out-of-bounds in array of length " ++ show n
