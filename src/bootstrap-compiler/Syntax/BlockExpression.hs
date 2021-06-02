module Syntax.BlockExpression where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.CurlyBrackets
import qualified Syntax.Ignored
import qualified Syntax.Common

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "block-expression")
analyzer = Syntax.Common.node do
    choice
        [ Syntax.Common.keyword "do"
        , Syntax.Common.keyword "repeat"
        , Syntax.Common.keyword "static-do"
        , Syntax.Common.keyword "static-repeat"
        ]

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ single '{'

    Syntax.CurlyBrackets.analyzer
