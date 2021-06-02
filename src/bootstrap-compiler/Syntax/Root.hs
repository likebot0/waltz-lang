module Syntax.Root where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Comment
import qualified Syntax.Expression
import qualified Syntax.Statement.BaseStatement
import qualified Syntax.Separator
import qualified Syntax.Common
import qualified Syntax.UnexpectedStatement
import qualified Syntax.Whitespace

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "root")
analyzer = Syntax.Common.node do
    Syntax.Common.between
        do optional $ lookAhead anySingle
        do eof
        do choice
            [ inject <$> Syntax.Expression.analyzer ","
            , inject <$> Syntax.Statement.BaseStatement.analyzer
            ]
        do choice
            [ Syntax.Comment.analyzer
            , Syntax.Separator.analyzer
            , Syntax.UnexpectedStatement.analyzer
            , Syntax.Whitespace.analyzer
            ]
