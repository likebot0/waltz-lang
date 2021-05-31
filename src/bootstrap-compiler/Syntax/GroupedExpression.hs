module Syntax.GroupedExpression where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import {-# SOURCE #-} qualified Syntax.Expression
import qualified Syntax.Ignored
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "grouped-expression")
analyzer = Syntax.Shared.node do
    single '('

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;)]}"

    expression <- Syntax.Expression.analyzer ")"

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do single ')'

    pure expression
