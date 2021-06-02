module Syntax.Literal.Object where

import Global
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Comment
import qualified Syntax.Discard
import qualified Syntax.KeyValue
import qualified Syntax.Separator
import qualified Syntax.Shared
import qualified Syntax.Statement.IfStatement
import qualified Syntax.Statement.LetStatement
import qualified Syntax.Statement.WithStatement
import qualified Syntax.UnexpectedStatement
import qualified Syntax.Whitespace

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "literal/object")
analyzer = Syntax.Shared.between
    do single '{'
    do single '}'
    do choice
        [ inject <$> do
            Syntax.Discard.analyzer "}"
        , inject <$> do
            Syntax.KeyValue.analyzer "}"
        , inject <$> do
            Syntax.Statement.IfStatement.analyzer "]"
        , inject <$> do
            Syntax.Statement.LetStatement.analyzer "]"
        , inject <$> do
            Syntax.Statement.WithStatement.analyzer "]"
        ]
    do choice
        [ Syntax.Comment.analyzer
        , Syntax.Separator.analyzer
        , Syntax.UnexpectedStatement.analyzer
        , Syntax.Whitespace.analyzer
        ]
