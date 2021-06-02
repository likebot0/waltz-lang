module Syntax.Statement.IfStatement where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Data.Maybe
import qualified Syntax.Analyzer
import {-# SOURCE #-} qualified Syntax.CurlyBrackets
import {-# SOURCE #-} qualified Syntax.Expression
import qualified Syntax.Ignored
import qualified Syntax.Separator
import qualified Syntax.Common

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "statement/if")
analyzer end = Syntax.Common.node do
    Syntax.Common.keyword "\\if"

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;)]}"

    expression <- Syntax.Expression.analyzer $ "," ++ end

    Syntax.Common.skipManyTill
        do choice
            [ Syntax.Separator.analyzer
            , Syntax.Ignored.analyzer
            ]
        do Syntax.Common.keyword "\\then"

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

    pure (expression, body)
