module Syntax.TypeExpression where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import {-# SOURCE #-} qualified Syntax.Expression
import qualified Syntax.Ignored
import qualified Syntax.Common

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "type-expression")
analyzer end = Syntax.Common.node do
    Syntax.Common.keyword "type"

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;)]}"

    Syntax.Expression.analyzer end
