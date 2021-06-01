module Syntax.Statement where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Statement.IfStatement
import qualified Syntax.Statement.LetStatement
import qualified Syntax.Statement.WithStatement

analyzer :: Syntax.Analyzer.WithEnd (Union (Ast.Statement "syntax-analyzed"))
analyzer end = choice
    [ inject <$> Syntax.Statement.IfStatement.analyzer end
    , inject <$> Syntax.Statement.LetStatement.analyzer end
    , inject <$> Syntax.Statement.WithStatement.analyzer end
    ]
