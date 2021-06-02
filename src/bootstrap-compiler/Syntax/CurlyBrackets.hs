module Syntax.CurlyBrackets where

import Global
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer

analyzer :: Syntax.Analyzer.Analyzer (Ast.CurlyBracketsBody "syntax-analyzed")
