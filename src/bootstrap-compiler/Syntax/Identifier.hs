module Syntax.Identifier where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Common

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "identifier")
analyzer = Syntax.Common.node do
    some $ noneOf "\\: \t#\r\n,;\"()[]{}"
