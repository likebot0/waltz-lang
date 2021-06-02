module Syntax.Discard where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Data.Maybe
import qualified Syntax.Analyzer
import {-# SOURCE #-} qualified Syntax.Expression
import qualified Syntax.Ignored
import qualified Syntax.Common

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "discard")
analyzer end = Syntax.Common.node do
    single ':'

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ choice
            [ () <$ oneOf end
            , () <$ noneOf "\\: \t;)]}"
            ]

    Syntax.Expression.analyzer $ "," ++ end
