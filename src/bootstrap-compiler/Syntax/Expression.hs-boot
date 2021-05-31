module Syntax.Expression where

import Global
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "expression")
