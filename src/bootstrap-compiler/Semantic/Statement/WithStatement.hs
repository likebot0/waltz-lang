module Semantic.Statement.WithStatement where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import {-# SOURCE #-} qualified Semantic.CurlyBracketsBody

analyze :: Semantic.Analyzer.Analyze "statement/with"
analyze x = do
    let body = Ast.children x

    Ast.Node
        <$> do fst <$> Semantic.CurlyBracketsBody.analyze body
        <*> do pure $ Ast.attributes x
