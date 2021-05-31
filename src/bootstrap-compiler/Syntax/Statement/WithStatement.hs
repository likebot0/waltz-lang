module Syntax.Statement.WithStatement where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.CurlyBrackets
import qualified Syntax.Ignored
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "statement/with")
analyzer end = Syntax.Shared.node do
    Syntax.Shared.keyword "\\with"

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ single '{'

    body <- Syntax.CurlyBrackets.analyzer

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ choice
            [ eof
            , () <$ oneOf ("#\r\n," ++ end)
            ]

    pure body
