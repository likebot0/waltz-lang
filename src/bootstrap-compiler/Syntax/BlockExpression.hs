module Syntax.BlockExpression where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.CurlyBrackets
import qualified Syntax.Ignored
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "block-expression")
analyzer = Syntax.Shared.node do
    choice
        [ Syntax.Shared.keyword "do"
        , Syntax.Shared.keyword "repeat"
        , Syntax.Shared.keyword "static-do"
        , Syntax.Shared.keyword "static-repeat"
        ]

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ single '{'

    Syntax.CurlyBrackets.analyzer
