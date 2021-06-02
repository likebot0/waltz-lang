module Syntax.Statement.Base where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Data.Maybe
import qualified Syntax.Analyzer
import qualified Syntax.Identifier
import qualified Syntax.Ignored
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "root/statement/base")
analyzer = Syntax.Shared.node do
    Syntax.Shared.keyword "\\base"

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;\"()[]{}"

    identifier <- Syntax.Identifier.analyzer

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ oneOf "#\r\n,"

    pure identifier
