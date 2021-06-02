module Syntax.Statement.IncludeStatement where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import {-# SOURCE #-} qualified Syntax.Expression
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "statement/include")
analyzer end = Syntax.Shared.node do
    Syntax.Shared.keyword "\\include"

    Syntax.Expression.analyzer end
