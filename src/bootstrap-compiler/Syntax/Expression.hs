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
import qualified Syntax.Shared
import qualified Syntax.TypeExpression
import qualified Syntax.Whitespace

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "expression")
analyzer end = Syntax.Shared.node do
    Syntax.Shared.between
        do lookAhead $ noneOf "\\: \t#\r\n,;)]}"
        do lookAhead $ choice
            [ eof
            , () <$ oneOf ("#\r\n" ++ end)
            ]
        do choice
            [ liftUnion <$> Syntax.BlockExpression.analyzer
            , liftUnion <$> Syntax.GroupedExpression.analyzer
            , liftUnion <$> Syntax.LambdaExpression.analyzer
            , liftUnion <$> Syntax.Literal.Array.analyzer
            , liftUnion <$> Syntax.Literal.Number.analyzer
            , liftUnion <$> Syntax.Literal.Object.analyzer
            , liftUnion <$> Syntax.Literal.String.analyzer
            , liftUnion <$> Syntax.TypeExpression.analyzer end
            , liftUnion <$> Syntax.Identifier.analyzer
            ]
        do choice
            [ Syntax.Whitespace.analyzer
            ]
