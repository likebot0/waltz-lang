module Syntax.KeyValue where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import {-# SOURCE #-} qualified Syntax.Expression
import qualified Syntax.Identifier
import qualified Syntax.Ignored
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "key-value")
analyzer end = Syntax.Shared.node do
    identifier <- Syntax.Identifier.analyzer

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do single ':'

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;)]}"

    expression <- Syntax.Expression.analyzer $ "," ++ end

    pure (identifier, expression)
