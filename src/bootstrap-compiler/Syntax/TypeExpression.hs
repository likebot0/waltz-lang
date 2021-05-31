module Syntax.TypeExpression where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import {-# SOURCE #-} qualified Syntax.Expression
import qualified Syntax.Ignored
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "type-expression")
analyzer end = Syntax.Shared.node do
    Syntax.Shared.keyword "type"

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;)]}"

    Syntax.Expression.analyzer end
