module Syntax.Expression where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.BlockExpression
import qualified Syntax.GroupedExpression
import qualified Syntax.Identifier
import qualified Syntax.LambdaExpression
import qualified Syntax.Literal.Array
import qualified Syntax.Literal.Number
import qualified Syntax.Literal.Object
import qualified Syntax.Literal.String
import qualified Syntax.Common
import qualified Syntax.TypeExpression
import qualified Syntax.Whitespace

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "expression")
analyzer end = Syntax.Common.node do
    Syntax.Common.between
        do lookAhead $ noneOf "\\: \t#\r\n,;)]}"
        do lookAhead $ choice
            [ eof
            , () <$ oneOf ("#\r\n" ++ end)
            ]
        do choice
            [ inject <$> Syntax.BlockExpression.analyzer
            , inject <$> Syntax.GroupedExpression.analyzer
            , inject <$> Syntax.LambdaExpression.analyzer
            , inject <$> Syntax.Literal.Array.analyzer
            , inject <$> Syntax.Literal.Number.analyzer
            , inject <$> Syntax.Literal.Object.analyzer
            , inject <$> Syntax.Literal.String.analyzer
            , inject <$> Syntax.TypeExpression.analyzer end
            , inject <$> Syntax.Identifier.analyzer
            ]
        do choice
            [ Syntax.Whitespace.analyzer
            ]
