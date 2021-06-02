module Syntax.Statement.IfStatement where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Data.Maybe
import qualified Syntax.Analyzer
import qualified Syntax.CurlyBrackets
import {-# SOURCE #-} qualified Syntax.Expression
import qualified Syntax.Ignored
import qualified Syntax.Separator
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "statement/if")
analyzer end = Syntax.Shared.node do
    Syntax.Shared.keyword "\\if"

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;)]}"

    expression <- Syntax.Expression.analyzer $ "," ++ end

    Syntax.Shared.skipManyTill
        do choice
            [ Syntax.Separator.analyzer
            , Syntax.Ignored.analyzer
            ]
        do Syntax.Shared.keyword "\\then"

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

    pure (expression, body)
