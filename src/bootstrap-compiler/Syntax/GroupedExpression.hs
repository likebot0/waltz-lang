module Syntax.GroupedExpression where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Common
import {-# SOURCE #-} qualified Syntax.Expression
import qualified Syntax.Ignored

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "grouped-expression")
analyzer = Syntax.Common.node do
    single '('

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;)]}"

    expression <- Syntax.Expression.analyzer ")"

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do single ')'

    pure expression
