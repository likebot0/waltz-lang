module Syntax.Statement.WithStatement where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import {-# SOURCE #-} qualified Syntax.CurlyBrackets
import qualified Syntax.Ignored
import qualified Syntax.Common

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "statement/with")
analyzer end = Syntax.Common.node do
    Syntax.Common.keyword "\\with"

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ single '{'

    body <- Syntax.CurlyBrackets.analyzer

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ choice
            [ eof
            , () <$ oneOf ("#\r\n," ++ end)
            ]

    pure body
