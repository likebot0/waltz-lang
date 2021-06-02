module Syntax.Identifier where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Punctuation
import qualified Syntax.Common
import qualified Syntax.Whitespace

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "identifier")
analyzer = Syntax.Common.node do
    some $ noneOf "\\: \t#\r\n,;\"()[]{}"
