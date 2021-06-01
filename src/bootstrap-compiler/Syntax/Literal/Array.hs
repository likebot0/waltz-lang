module Syntax.Literal.Array where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import {-# SOURCE #-} qualified Syntax.Expression
import {-# SOURCE #-} qualified Syntax.Statement
import qualified Syntax.Comment
import qualified Syntax.Discard
import qualified Syntax.Separator
import qualified Syntax.Shared
import qualified Syntax.UnexpectedStatement
import qualified Syntax.Whitespace

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "literal/array")
analyzer = Syntax.Shared.node do
    Syntax.Shared.between
        do single '['
        do single ']'
        do choice
            [ inject <$> do
                Syntax.Discard.analyzer "]"
            , inject <$> do
                Syntax.Expression.analyzer ",]"
            , reinterpret <$> do
                Syntax.Statement.analyzer "]"
            ]
        do choice
            [ Syntax.Comment.analyzer
            , Syntax.Separator.analyzer
            , Syntax.UnexpectedStatement.analyzer
            , Syntax.Whitespace.analyzer
            ]
