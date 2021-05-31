module Syntax.LambdaExpression where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Data.Maybe
import qualified Syntax.Analyzer
import qualified Syntax.CurlyBrackets
import qualified Syntax.Identifier
import qualified Syntax.Ignored
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "lambda-expression")
analyzer = Syntax.Shared.node do
    Syntax.Shared.keyword "fun"

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;\"()[]}"

    identifier <- optional Syntax.Identifier.analyzer

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ single '{'

    (,)
        identifier
        <$> Syntax.CurlyBrackets.analyzer
