module Syntax.LambdaExpression where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Common
import qualified Syntax.CurlyBrackets
import qualified Syntax.Identifier
import qualified Syntax.Ignored

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "lambda-expression")
analyzer = Syntax.Common.node do
    Syntax.Common.keyword "fun"

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;\"()[]}"

    identifier <- optional Syntax.Identifier.analyzer

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ single '{'

    (,)
        identifier
        <$> Syntax.CurlyBrackets.analyzer
