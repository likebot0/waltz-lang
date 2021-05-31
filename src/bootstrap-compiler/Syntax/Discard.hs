module Syntax.Discard where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Data.Maybe
import qualified Syntax.Analyzer
import {-# SOURCE #-} qualified Syntax.Expression
import qualified Syntax.Ignored
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "discard")
analyzer end = Syntax.Shared.node do
    single ':'

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ choice
            [ () <$ oneOf end
            , () <$ noneOf "\\: \t;)]}"
            ]

    optional $ Syntax.Expression.analyzer $ "," ++ end
