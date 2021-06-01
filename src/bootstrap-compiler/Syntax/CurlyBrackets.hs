module Syntax.CurlyBrackets where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Comment
import qualified Syntax.Discard
import qualified Syntax.KeyValue
import qualified Syntax.Separator
import qualified Syntax.Shared
import {-# SOURCE #-} qualified Syntax.Statement
import qualified Syntax.UnexpectedStatement
import qualified Syntax.Whitespace

analyzer :: Syntax.Analyzer.Analyzer (Ast.CurlyBracketsBody "syntax-analyzed")
analyzer =
    Syntax.Shared.between
        do single '{'
        do single '}'
        do choice
            [ inject <$> do
                Syntax.Discard.analyzer "}"
            , inject <$> do
                Syntax.KeyValue.analyzer "}"
            , reinterpret <$> do
                Syntax.Statement.analyzer "}"
            ]
        do choice
            [ Syntax.Comment.analyzer
            , Syntax.Separator.analyzer
            , Syntax.UnexpectedStatement.analyzer
            , Syntax.Whitespace.analyzer
            ]
