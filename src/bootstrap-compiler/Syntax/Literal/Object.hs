module Syntax.Literal.Object where

import Global
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.CurlyBrackets
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "literal/object")
analyzer = Syntax.Shared.node Syntax.CurlyBrackets.analyzer
