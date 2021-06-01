module Syntax.Root where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Comment
import qualified Syntax.Expression
import qualified Syntax.Root.Statement
import qualified Syntax.Separator
import qualified Syntax.Shared
import qualified Syntax.UnexpectedStatement
import qualified Syntax.Whitespace

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "root")
analyzer = Syntax.Shared.node do
    Syntax.Shared.between
        do optional $ lookAhead anySingle
        do eof
        do choice
            [ inject <$> do
                Syntax.Expression.analyzer ","
            , reUnion <$> do
                Syntax.Root.Statement.analyzer
            ]
        do choice
            [ Syntax.Comment.analyzer
            , Syntax.Separator.analyzer
            , Syntax.UnexpectedStatement.analyzer
            , Syntax.Whitespace.analyzer
            ]
