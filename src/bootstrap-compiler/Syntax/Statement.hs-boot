module Syntax.Statement where

import Global
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer

analyzer :: Syntax.Analyzer.WithEnd (Union (Ast.Statement "syntax-analyzed"))
