module Syntax.Statement.BaseStatement where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Common
import qualified Syntax.Identifier
import qualified Syntax.Ignored

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "statement/base")
analyzer = Syntax.Common.node do
    Syntax.Common.keyword "\\base"

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;\"()[]{}"

    identifier <- Syntax.Identifier.analyzer

    Syntax.Common.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ oneOf "#\r\n,"

    pure identifier
