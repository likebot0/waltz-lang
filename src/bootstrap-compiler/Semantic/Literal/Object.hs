module Semantic.Literal.Object where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import {-# SOURCE #-} qualified Semantic.CurlyBracketsBody

analyze :: Semantic.Analyzer.Analyze "literal/object"
analyze x = do
    (ast, memberStore) <- Semantic.CurlyBracketsBody.analyze $ Ast.children x

    pure $ Ast.Node
        do Ast.Semantic.CurlyBracketsAttributes
            do Ast.Syntax.location $ Ast.attributes x
            memberStore
        ast
