module Syntax.Statement.LetStatement where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Data.Maybe
import qualified Syntax.Analyzer
import qualified Syntax.KeyValue
import qualified Syntax.Ignored
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.WithEnd (Ast.Node "syntax-analyzed" "statement/let")
analyzer end = Syntax.Shared.node do
    Syntax.Shared.keyword "\\let"

    Syntax.Shared.skipManyTill
        do Syntax.Ignored.analyzer
        do lookAhead $ noneOf "\\: \t#\r\n,;\"()[]{}"

    Syntax.KeyValue.analyzer end
